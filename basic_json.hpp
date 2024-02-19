#include <cassert>
#include <iterator>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>
#include <concepts>
#include <array>
#include <ranges>

// https://www.rfc-editor.org/rfc/rfc8259

namespace bizwen
{
	struct nulljson_t
	{
		explicit constexpr nulljson_t() noexcept = default;
	};

	inline constexpr nulljson_t nulljson{};

	template <typename Number = double,
	    typename Integer = long long, typename UInteger = unsigned long long, typename Allocator = std::allocator<char>>
	class basic_json_node;

	template <typename Node = basic_json_node<>, typename String = std::string,
	    typename Array = std::vector<Node>,
	    typename Object = std::map<String, Node>,
	    bool HasInteger = true, bool HasUInteger = true>
	class basic_json;

	template <typename Node = basic_json_node<>, typename String = std::string,
	    typename Array = std::vector<Node>,
	    typename Object = std::map<String, Node>,
	    bool HasInteger = true, bool HasUInteger = true>
	class basic_const_json_slice;

	template <typename Node = basic_json_node<>, typename String = std::string,
	    typename Array = std::vector<Node>,
	    typename Object = std::map<String, Node>,
	    bool HasInteger = true, bool HasUInteger = true>
	class basic_json_slice;

	// https://cplusplus.github.io/LWG/issue3917
	// since the types of string, array and map are unknown at this point,
	// memory allocation can only be done by instantiating char.
	template <typename Number,
	    typename Integer, typename UInteger, typename Allocator>
	class basic_json_node: protected Allocator
	{
	public:
		using number_type = Number;
		using integer_type = Integer;
		using uinteger_type = UInteger;
		using allocator_type = Allocator;

	private:
		static_assert(std::floating_point<number_type>);
		static_assert(std::signed_integral<integer_type>);
		static_assert(std::unsigned_integral<uinteger_type>);

		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		friend class basic_json;

		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		friend class basic_const_json_slice;

		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		friend class basic_json_slice;

		enum class kind_t : unsigned char
		{
			empty,
			null,
			true_value,
			false_value,
			number,
			integer,
			uinteger,
			string,
			array,
			object
		};

		union stor_t
		{
			void* str_;
			void* arr_;
			void* obj_;
			number_type num_;
			integer_type int_;
			uinteger_type uint_;
		};

		stor_t stor_{};
		kind_t kind_{};

	public:
		constexpr basic_json_node() noexcept = default;
		constexpr basic_json_node(basic_json_node const&) = default;
		constexpr basic_json_node(basic_json_node&& rhs) noexcept = default;
		constexpr basic_json_node& operator=(basic_json_node&& rhs) noexcept = default;
	};

	template <typename Node, typename String,
	    typename Array,
	    typename Object,
	    bool HasInteger, bool HasUInteger>
	class basic_json_slice: public basic_const_json_slice<Node, String, Array, Object, HasInteger, HasUInteger>
	{
	public:
		using const_slice_type = basic_const_json_slice<Node, String, Array, Object, HasInteger, HasUInteger>;

		using const_slice_type::has_integer;
		using const_slice_type::has_uinteger;

		using typename const_slice_type::node_type;
		using typename const_slice_type::object_type;
		using typename const_slice_type::value_type;
		using typename const_slice_type::array_type;
		using typename const_slice_type::string_type;

		using typename const_slice_type::slice_type;
		// using typename const_slice_type::const_slice_type;
		using typename const_slice_type::json_type;

		using typename const_slice_type::number_type;
		using typename const_slice_type::integer_type;
		using typename const_slice_type::uinteger_type;

		using typename const_slice_type::char_type;
		using typename const_slice_type::map_node_type;
		using typename const_slice_type::allocator_type;
		using typename const_slice_type::key_string_type;
		using typename const_slice_type::key_char_type;

	private:
		friend json_type;
		friend const_slice_type;

		using typename const_slice_type::kind_t;
		using typename const_slice_type::stor_t;

		using const_slice_type::json_;

		using const_slice_type::stor;

		constexpr stor_t& stor() noexcept
		{
			return json_->node_.stor_;
		}

	public:
		using const_slice_type::empty;
		using const_slice_type::string;
		using const_slice_type::null;
		using const_slice_type::boolean;
		using const_slice_type::number;
		using const_slice_type::object;
		using const_slice_type::array;
		using const_slice_type::integer;
		using const_slice_type::uinteger;

		constexpr void swap(basic_json_slice& rhs) noexcept
		{
			auto temp = json_;
			json_ = rhs.json_;
			rhs.json_ = temp;
		}

		friend constexpr void swap(basic_json_slice& lhs, basic_json_slice& rhs) noexcept
		{
			lhs.swap(rhs);
		}

		// similar to iterators, default construction is allowed, but except for operator=,
		// operations on default-constructed slice cause undefined behavior.
		constexpr basic_json_slice() noexcept = default;

		constexpr basic_json_slice(basic_json_slice&& rhs) noexcept = default;

		constexpr basic_json_slice(basic_json_slice const& rhs) noexcept = default;

		constexpr basic_json_slice(json_type& j) noexcept
		    : const_slice_type(j)
		{
		}

		constexpr basic_json_slice(node_type& n) noexcept
		    : const_slice_type(n)
		{
		}

		constexpr basic_json_slice& operator=(basic_json_slice const& rhs) noexcept = default;

		constexpr basic_json_slice& operator=(basic_json_slice&& rhs) noexcept = default;

		constexpr explicit operator string_type&() &
		{
			if (!string())
				throw std::runtime_error("json error: value isn't a string.");

			return *static_cast<string_type*>(stor().str_);
		}

		constexpr explicit operator array_type&() &
		{
			if (!array())
				throw std::runtime_error("json error: value isn't an array.");

			return *static_cast<array_type*>(stor().arr_);
		}

		constexpr explicit operator object_type&() &
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object.");

			return *static_cast<object_type*>(stor().obj_);
		}

	private:
		constexpr void allocate_object()
		{
			typename json_type::template alloc_guard_<object_type> guard(*json_);
			stor().obj_ = new (guard.get()) object_type();
			guard.release();
			(*json_).kind(kind_t::object);
		}

	public:
		using const_slice_type::operator[];

		constexpr basic_json_slice operator[](key_string_type const& k)
		{
			auto e = empty();

			if (!object() && !e)
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			if (e)
				allocate_object();

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto [i, _] = o.emplace(k, node_type{});
			auto& [_, v] = *i;

			return v;
		}

		template <typename KeyStrLike>
		    requires std::convertible_to<KeyStrLike, key_string_type> // wrong, need check transparent comparable
		    && (std::is_convertible_v<KeyStrLike const&, key_char_type const*> == false)
		constexpr basic_json_slice operator[](KeyStrLike const& k)
		{
			auto e = empty();

			if (!object() && !e)
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			if (e)
				allocate_object();

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto [i, _] = o.emplace(k, node_type{});
			auto& [_, v] = *i;

			return v;
		}

		constexpr basic_json_slice operator[](key_char_type const* k)
		{
			auto e = empty();

			if (!object() && !e)
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			if (e)
				allocate_object();

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto [i, _] = o.emplace(k, node_type{});
			auto& [_, v] = *i;

			return v;
		}

		constexpr basic_json_slice operator[](array_type::size_type pos)
		{
			if (!array())
				throw std::runtime_error("json error: value isn't an array but is accessed using operator[].");

			auto const& a = *static_cast<array_type const*>(stor().arr_);

			return a[pos];
		}

		constexpr basic_json_slice& operator=(string_type const& str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string || !is_empty)
				throw std::runtime_error("json error: current value is not empty or not a string.");

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = str;
			}
			else // empty
			{
				typename json_type::template alloc_guard_<string_type> guard(*json_);
				stor().str_ = new (guard.get()) string_type(str);
				guard.release();
				(*json_).kind(kind_t::string);
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(string_type&& str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string || !is_empty)
				throw std::runtime_error("json error: current value is not empty or not a string.");

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = std::move(str);
			}
			else // empty
			{
				typename json_type::template alloc_guard_<string_type> guard(*json_);
				stor().str_ = new (guard.get()) string_type(std::move(str));
				guard.release();
				(*json_).kind(kind_t::string);
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(char_type const* str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string || !is_empty)
				throw std::runtime_error("json error: current value is not empty or not a string.");

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = str;
			}
			else // empty
			{
				typename json_type::template alloc_guard_<string_type> guard(*json_);
				stor().str_ = new (guard.get()) string_type(str);
				guard.release();
				(*json_).kind(kind_t::string);
			}

			return *this;
		}

		template <typename StrLike>
		    requires std::constructible_from<string_type, StrLike>
		    && (std::is_convertible_v<StrLike const&, char_type const*> == false)
		constexpr basic_json_slice& operator=(StrLike const& str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string || !is_empty)
				throw std::runtime_error("json error: current value is not empty or not a string.");

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = str;
			}
			else
			{
				typename json_type::template alloc_guard_<string_type> guard(*json_);
				stor().str_ = new (guard.get()) string_type(str);
				guard.release();
				(*json_).kind(kind_t::string);
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(nulljson_t n)
		{
			bool is_null = null();
			bool is_empty = empty();

			if (!is_null || !is_empty)
				throw std::runtime_error("json error: current value is not empty or not null.");

			if (is_empty)
				(*json_).kind(kind_t::null);

			return *this;
		}

		template <typename T>
		    requires std::is_arithmetic_v<T>
		constexpr basic_json_slice& operator=(T n)
		{
			if constexpr (std::same_as<T, bool>)
			{
				bool is_boolean = boolean();
				bool is_empty = empty();

				if (!is_boolean && !is_empty)
					throw std::runtime_error("json error: current value is not empty or not bool.");

				if (is_empty)
					(*json_).kind(n ? kind_t::true_value : kind_t::false_value);
			}
			else if constexpr (HasInteger && std::signed_integral<T>)
			{
				bool is_number = number() || uinteger() || integer();
				bool is_empty = empty();

				if (!is_number && !is_empty)
					throw std::runtime_error("json error: current value is not empty or not a number.");

				(*json_).stor().int_ = n;
				(*json_).kind(kind_t::integer);
			}
			else if constexpr (HasUInteger && std::unsigned_integral<T>)
			{
				bool is_number = number() || uinteger() || integer();
				bool is_empty = empty();

				if (!is_number && !is_empty)
					throw std::runtime_error("json error: current value is not empty or not a number.");

				(*json_).stor().uint_ = n;
				(*json_).kind(kind_t::uinteger);
			}
			else // fallback
			{
				bool is_number = number() || uinteger() || integer();
				bool is_empty = empty();

				if (!is_number && !is_empty)
					throw std::runtime_error("json error: current value is not empty or not a number.");

				(*json_).stor().num_ = n;
				(*json_).kind(kind_t::number);
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(json_type& j)
		{
			json_ = &j;

			return *this;
		}

		constexpr basic_json_slice& operator=(node_type& n)
		{
			json_ = reinterpret_cast<json_type*>(&n);

			return *this;
		}

	private:
		static constexpr basic_json_slice node_to_slice(node_type & node) noexcept
		{
			return node;
		}

		static constexpr std::pair<key_string_type const&, basic_json_slice> pair_node_to_slice(map_node_type & pair) noexcept
		{
			auto& [key, value]{ pair };
			return { key, value };
		}

	public:
		using const_slice_type::as_array;

		constexpr auto as_array()
		{
			return static_cast<array_type&>(*this) | std::views::transform(node_to_slice);
		}

		using const_slice_type::as_object;

		constexpr auto as_object()
		{
			return static_cast<object_type&>(*this) | std::views::transform(pair_node_to_slice);
		}
	};

	template <typename Node, typename String,
	    typename Array,
	    typename Object,
	    bool HasInteger, bool HasUInteger>
	class basic_const_json_slice
	{
	public:
		static inline constexpr bool has_integer = HasInteger;
		static inline constexpr bool has_uinteger = HasUInteger;

		using node_type = Node;
		using object_type = Object;
		using value_type = Node;
		using array_type = Array;
		using string_type = String;

		using const_slice_type = basic_const_json_slice;
		using slice_type = basic_json_slice<node_type, string_type, array_type, object_type, has_integer, has_uinteger>;
		using json_type = basic_json<node_type, string_type, array_type, object_type, has_integer, has_uinteger>;

		using number_type = node_type::number_type;
		using integer_type = node_type::integer_type;
		using uinteger_type = node_type::uinteger_type;

		using char_type = string_type::value_type;
		using map_node_type = object_type::value_type;
		using allocator_type = node_type::allocator_type;
		using key_string_type = object_type::key_type;
		using key_char_type = key_string_type::value_type;

	private:
		using traits_t = std::allocator_traits<allocator_type>;
		using kind_t = node_type::kind_t;
		using stor_t = node_type::stor_t;

		friend json_type;
		friend slice_type;

		json_type* json_{};

		constexpr kind_t kind() const noexcept
		{
			assert(json_);

			return json_->node_.kind_;
		}

		constexpr stor_t const& stor() const noexcept
		{
			return json_->node_.stor_;
		}

	public:
		[[nodiscard]] constexpr bool empty() const noexcept
		{
			return kind() == kind_t::empty;
		}

		[[nodiscard]] constexpr bool string() const noexcept
		{
			return kind() == kind_t::string;
		}

		[[nodiscard]] constexpr bool null() const noexcept
		{
			return kind() == kind_t::null;
		}

		[[nodiscard]] constexpr bool boolean() const noexcept
		{
			auto k = kind();

			return k == kind_t::true_value || k == kind_t::false_value;
		}

		[[nodiscard]] constexpr bool number() const noexcept
		{
			auto k = kind();

			bool is_integer{};
			bool is_uinteger{};

			if constexpr (HasInteger)
			{
				is_integer = k == kind_t::integer;
			}

			if constexpr (HasUInteger)
			{
				is_uinteger = k == kind_t::uinteger;
			}

			return k == kind_t::number || is_integer || is_uinteger;
		}

		[[nodiscard]] constexpr bool object() const noexcept
		{
			return kind() == kind_t::object;
		}

		[[nodiscard]] constexpr bool array() const noexcept
		{
			return kind() == kind_t::array;
		}

		[[nodiscard]] constexpr bool integer() const noexcept
		    requires HasInteger
		{
			return kind() == kind_t::integer;
		}

		[[nodiscard]] constexpr bool uinteger() const noexcept
		    requires HasUInteger
		{
			return kind() == kind_t::uinteger;
		}

		constexpr void swap(basic_const_json_slice& rhs) noexcept
		{
			auto temp = json_;
			json_ = rhs.json_;
			rhs.json_ = temp;
		}

		friend constexpr void swap(basic_const_json_slice& lhs, basic_const_json_slice& rhs) noexcept
		{
			lhs.swap(rhs);
		}

		// similar to iterators, default construction is allowed, but except for operator=,
		// operations on default-constructed slice cause undefined behavior.
		constexpr basic_const_json_slice() noexcept = default;

		constexpr basic_const_json_slice(basic_const_json_slice&& rhs) noexcept = default;

		constexpr basic_const_json_slice(basic_const_json_slice const& rhs) noexcept = default;

		constexpr basic_const_json_slice(json_type const& j) noexcept
		    : json_(const_cast<json_type*>(&j))
		{
		}

		constexpr basic_const_json_slice(node_type const& n) noexcept
		    : json_(const_cast<json_type*>(reinterpret_cast<json_type const*>(&n)))
		{
		}

		constexpr basic_const_json_slice(slice_type const& s) noexcept
		    : json_(s.json_)
		{
		}

		constexpr basic_const_json_slice& operator=(basic_const_json_slice const& rhs) noexcept = default;

		constexpr basic_const_json_slice& operator=(basic_const_json_slice&& rhs) noexcept = default;

		constexpr explicit operator bool() const
		{
			if (!boolean())
				throw std::runtime_error("json error: value not a boolean.");

			auto k = kind();

			return k == kind_t::true_value ? 1 : 0;
		}

		constexpr explicit operator number_type() const
		{
			auto k = kind();
			auto s = stor();

			if (k == kind_t::number)
				return s.num_;
			else if (k == kind_t::integer)
				return s.int_;
			else if (k == kind_t::uinteger)
				return s.uint_;

			throw std::runtime_error("json error: value isn't a number.");
		}

		constexpr explicit operator nulljson_t() const
		{
			if (!null())
				throw std::runtime_error("json error: value isn't a null.");

			return nulljson;
		}

		constexpr explicit operator string_type const&() const&
		{
			if (!string())
				throw std::runtime_error("json error: value isn't a string.");

			return *static_cast<string_type const*>(stor().str_);
		}

		constexpr explicit operator array_type const&() const&
		{
			if (!array())
				throw std::runtime_error("json error: value isn't an array.");

			return *static_cast<array_type const*>(stor().arr_);
		}

		constexpr explicit operator object_type const&() const&
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object.");

			return *static_cast<object_type const*>(stor().obj_);
		}

		constexpr explicit operator integer_type() const
		    requires HasInteger
		{
			if (!integer())
				throw std::runtime_error("json error: value isn't an integer.");

			return stor().int_;
		}

		constexpr explicit operator uinteger_type() const
		    requires HasUInteger
		{
			if (!uinteger())
				throw std::runtime_error("json error: value isn't an unsigned integer.");

			return stor().uint_;
		}

		constexpr basic_const_json_slice operator[](key_string_type const& k) const
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto& [_, v] = *i;

			return v;
		}

		template <typename KeyStrLike>
		    requires std::convertible_to<KeyStrLike, key_string_type> // wrong, need check transparent comparable
		    && (std::is_convertible_v<KeyStrLike const&, key_char_type const*> == false)
		constexpr basic_const_json_slice operator[](KeyStrLike const& k) const
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto& [_, v] = *i;

			return v;
		}

		constexpr basic_const_json_slice operator[](key_char_type const* k) const
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto& [_, v] = *i;

			return v;
		}

		constexpr basic_const_json_slice operator[](array_type::size_type pos) const
		{
			if (!array())
				throw std::runtime_error("json error: value isn't an array but is accessed using operator[].");

			auto& a = *static_cast<array_type*>(stor().arr_);

			return a[pos];
		}

	private:
		static constexpr basic_const_json_slice node_to_slice(node_type const& node) noexcept
		{
			return node;
		}

		static constexpr std::pair<key_string_type const&, basic_const_json_slice> pair_node_to_slice(map_node_type const& pair) noexcept
		{
			auto& [key, value]{ pair };
			return { key, value };
		}

	public:
		constexpr auto as_array() const
		{
			return static_cast<array_type const&>(*this) | std::views::transform(node_to_slice);
		}

		constexpr auto as_object() const
		{
			return static_cast<object_type const&>(*this) | std::views::transform(pair_node_to_slice);
		}
	};

	template <typename Node, typename String,
	    typename Array,
	    typename Object,
	    bool HasInteger, bool HasUInteger>
	class basic_json
	{
	public:
		static inline constexpr bool has_integer = HasInteger;
		static inline constexpr bool has_uinteger = HasUInteger;

		using node_type = Node;
		using object_type = Object;
		using value_type = Node;
		using array_type = Array;
		using string_type = String;

		using slice_type = basic_json_slice<node_type, string_type, array_type, object_type, HasInteger, HasUInteger>;
		using const_slice_type = basic_const_json_slice<node_type, string_type, array_type, object_type, HasInteger, HasUInteger>;

		using number_type = node_type::number_type;
		using integer_type = node_type::integer_type;
		using uinteger_type = node_type::uinteger_type;

		using char_type = string_type::value_type;
		using map_node_type = object_type::value_type;
		using allocator_type = node_type::allocator_type;
		using key_string_type = object_type::key_type;
		using key_char_type = key_string_type::value_type;

	private:
		using traits_t = std::allocator_traits<allocator_type>;
		using kind_t = node_type::kind_t;
		using stor_t = node_type::stor_t;

		friend const_slice_type;
		friend slice_type;

		static_assert(std::integral<char_type>);
		static_assert(std::integral<key_char_type>);
		static_assert(std::floating_point<number_type>);
		static_assert(std::signed_integral<integer_type>);
		static_assert(std::unsigned_integral<uinteger_type>);
		static_assert(sizeof(integer_type) == sizeof(uinteger_type));
		static_assert(std::same_as<node_type, typename array_type::value_type>);
		static_assert(std::same_as<node_type, typename object_type::mapped_type>);
		static_assert(std::random_access_iterator<typename array_type::iterator>);
		static_assert(std::random_access_iterator<typename string_type::iterator>);
		static_assert(std::bidirectional_iterator<typename object_type::iterator>);
		static_assert(std::random_access_iterator<typename key_string_type::iterator>);
		static_assert(std::same_as<basic_json_node<number_type, integer_type, uinteger_type>, node_type>);

		node_type node_;

		constexpr void kind(kind_t k) noexcept { node_.kind_ = k; }

		constexpr kind_t kind() const noexcept
		{
			return node_.kind_;
		}

		constexpr stor_t& stor() noexcept
		{
			return node_.stor_;
		}

		constexpr stor_t const& stor() const noexcept
		{
			return node_.stor_;
		}

		template <typename T>
		constexpr T* alloc()
		{
			return typename traits_t::template rebind_alloc<T>(node_).allocate(1);
		}

		template <typename T>
		constexpr void dealloc(T* p) noexcept
		{
			return typename traits_t::template rebind_alloc<T>(node_).deallocate(p, 1);
		}

		void destroy() noexcept
		{
			auto k = kind();
			auto& s = stor();

			switch (k)
			{
			case kind_t::string: {
				auto p = static_cast<string_type*>(s.str_);
				(*p).~string_type();
				dealloc(static_cast<string_type*>(s.str_));

				break;
			}
			case kind_t::array: {
				auto p = static_cast<array_type*>(s.arr_);

				for (auto&& i : *p)
				{
					auto&& json = basic_json(std::move(i));
					json.destroy();
				}

				(*p).~array_type();
				dealloc(p);

				break;
			}
			case kind_t::object: {
				auto p = static_cast<object_type*>(s.obj_);

				for (auto&& [_, v] : *p)
				{
					auto&& json = basic_json(std::move(v));
					json.destroy();
				}

				(*p).~object_type();
				dealloc(p);

				break;
			}
			default: {
				// make clang happy
			}
			}
		}

		template <typename T>
		struct alloc_guard_
		{
			T* ptr;
			basic_json& json;

			constexpr alloc_guard_() noexcept = delete;

			constexpr alloc_guard_(alloc_guard_ const&) noexcept = delete;

			constexpr alloc_guard_(basic_json& j)
			    : json(j)
			{
				ptr = json.alloc<T>();
			}

			constexpr void release() noexcept
			{
				ptr = nullptr;
			}

			constexpr T* get() noexcept
			{
				assert(ptr);

				return ptr;
			}

			constexpr ~alloc_guard_()
			{
				if (ptr)
					json.dealloc(ptr);
			}
		};

		struct rollbacker_array_part_
		{
			array_type::iterator& sentry;
			array_type& array;

			constexpr rollbacker_array_part_() noexcept = delete;

			constexpr rollbacker_array_part_(rollbacker_array_part_ const&) noexcept = delete;

			constexpr rollbacker_array_part_(array_type::iterator& i, array_type& a) noexcept
			    : sentry(i)
			    , array(a)
			{
			}

			constexpr ~rollbacker_array_part_()
			{
				auto end = array.end();

				if (sentry == end)
					return;

				for (auto begin = array.begin(); begin != sentry; ++begin)
				{
					reinterpret_cast<basic_json&>(*begin).destroy();
				}
			}
		};

		struct rollbacker_array_all_
		{
			array_type& array;
			bool done;

			constexpr rollbacker_array_all_() noexcept = delete;

			constexpr rollbacker_array_all_(rollbacker_array_all_ const&) noexcept = delete;

			constexpr void release() noexcept
			{
				done = true;
			}

			constexpr rollbacker_array_all_(array_type& a) noexcept
			    : array(a)
			    , done(false)
			{
			}

			constexpr ~rollbacker_array_all_()
			{
				if (done)
					return;

				auto end = array.end();

				for (auto begin = array.begin(); begin != end; ++begin)
				{
					reinterpret_cast<basic_json&>(*begin).destroy();
				}
			}
		};

		struct rollbacker_map_all_
		{
			object_type& object;
			bool done;

			constexpr void release() noexcept
			{
				done = true;
			}

			constexpr rollbacker_map_all_(object_type& o) noexcept
			    : object(o)
			    , done(false)
			{
			}

			constexpr ~rollbacker_map_all_()
			{
				if (done)
					return;

				auto end = object.end();
				auto begin = object.begin();

				for (auto begin = object.begin(); begin != end; ++begin)
				{
					auto&& [key, value] = *begin;
					reinterpret_cast<basic_json&>(value).destroy();
				}
			}
		};

	public:
		constexpr void swap(basic_json& rhs) noexcept
		{
			{
				auto temp = stor();
				stor() = rhs.stor();
				rhs.stor() = temp;
			}
			{
				auto temp = kind();
				kind(rhs.kind());
				rhs.kind(temp);
			}
		}

		friend constexpr void swap(basic_json& lhs, basic_json& rhs) noexcept
		{
			lhs.swap(rhs);
		}

		constexpr basic_json() noexcept = default;

		constexpr basic_json(basic_json&& rhs) noexcept
		{
			rhs.swap(*this);
		}
		constexpr basic_json(basic_json const& rhs)
		{
			clone(rhs);
		}

		constexpr basic_json& operator=(basic_json const& rhs)
		{
			destroy();
			clone(rhs);

			return *this;
		}

		constexpr basic_json& operator=(basic_json&& rhs)
		{
			rhs.swap(*this);

			return *this;
		}

		constexpr basic_json(decltype(nullptr)) noexcept = delete; // prevent implicit construct string

		template <typename T>
		    requires std::is_arithmetic_v<T>
		constexpr basic_json(T n) noexcept
		{
			if constexpr (std::same_as<T, bool>)
			{
				kind(n ? kind_t::true_value : kind_t::false_value);
			}
			else if constexpr (HasInteger && std::signed_integral<T>)
			{
				stor().int_ = n;
				kind(kind_t::integer);
			}
			else if constexpr (HasUInteger && std::unsigned_integral<T>)
			{
				stor().uint_ = n;
				kind(kind_t::uinteger);
			}
			else // fallback
			{
				stor().num_ = n;
				kind(kind_t::number);
			}
		}

		constexpr explicit basic_json(string_type v)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(std::move(v));
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(char_type const* begin, char_type const* end)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(begin, end);
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(char_type const* str, string_type::size_type count)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(str, count);
			guard.release();
			kind(kind_t::string);
		}

		constexpr explicit basic_json(char_type const* str)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(str);
			guard.release();
			kind(kind_t::string);
		}

		template <typename StrLike>
		    requires std::constructible_from<string_type, StrLike>
		    && (std::is_convertible_v<StrLike const&, char_type const*> == false)
		constexpr basic_json(StrLike const& str)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(str);
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(array_type arr)
		{
			rollbacker_array_all_ rollbacker(arr);
			alloc_guard_<array_type> guard(*this);
			stor().arr_ = new (guard.get()) array_type(std::move(arr));
			guard.release();
			rollbacker.release();
			kind(kind_t::array);
		}

		constexpr basic_json(object_type obj)
		{
			rollbacker_map_all_ rollbacker(obj);
			alloc_guard_<object_type> guard(*this);
			stor().arr_ = new (guard.get()) object_type(std::move(obj));
			guard.release();
			rollbacker.release();
			kind(kind_t::object);
		}

		constexpr basic_json(node_type&& n) noexcept
		{
			kind(kind_t{});
			stor() = stor_t{};
			reinterpret_cast<node_type&>(*this) = std::move(n);
		}

		[[nodiscard("discard nodes will cause leaks")]] constexpr operator node_type() && noexcept
		{
			auto node = reinterpret_cast<node_type&>(*this);
			kind(kind_t{});
			stor() = {};

			return node;
		}

	private:
		constexpr void clone(const basic_json& rhs)
		{
			auto rk = rhs.kind();
			auto const& rs = rhs.stor();
			auto& s = stor();

			switch (rk)
			{
			case kind_t::string: {
				auto const& rstr = *static_cast<string_type const*>(rs.str_);
				alloc_guard_<string_type> mem(*this);
				auto lptr = mem.get();
				lptr = new (lptr) string_type(rstr);
				mem.release();
				s.str_ = lptr;
				break;
			}
			case kind_t::array: {
				if constexpr (std::is_trivially_copyable_v<allocator_type>)
				{
					auto const& rarr = *static_cast<array_type const*>(rs.arr_);
					alloc_guard_<array_type> guard(*this);
					auto lptr = new (guard.get()) array_type(rarr);
					array_type& larr{ *lptr };
					auto sentry = larr.begin();
					rollbacker_array_part_ rollbacker(sentry, larr);

					auto first = rarr.begin();
					auto last = rarr.end();
					for (; first != last; ++sentry, ++first)
					{
						reinterpret_cast<basic_json&>(*sentry).clone(reinterpret_cast<basic_json const&>(*first));
					}

					guard.release();
					s.arr_ = lptr;
				}
				else
				{
					auto const& rarr = *static_cast<array_type const*>(rs.arr_);
					alloc_guard_<array_type> guard(*this);
					auto lptr = new (guard.get()) array_type();
					array_type& larr{ *lptr };
					larr.reserve(rarr.size());
					rollbacker_array_all_ rollbacker(larr);

					auto first = rarr.begin();
					auto last = rarr.end();
					for (; first != last; ++first)
					{
						larr.push_back(basic_json{ reinterpret_cast<basic_json const&>(*first) });
					}

					guard.release();
					s.arr_ = lptr;
				}
				break;
			}
			case kind_t::object: {
				auto const& robj = *static_cast<object_type const*>(rs.obj_);
				alloc_guard_<object_type> guard(*this);
				auto lptr = new (guard.get()) object_type();
				object_type& lobj{ *lptr };
				rollbacker_map_all_ rollbacker(lobj);

				auto first = robj.begin();
				auto last = robj.end();

				for (; first != last; ++first)
				{
					auto const& [rkey, rvalue] = *first;
					basic_json temp;
					temp.clone(reinterpret_cast<basic_json const&>(rvalue));
					lobj.emplace(rkey, reinterpret_cast<node_type&&>(std::move(temp)));
				}

				rollbacker.release();
				guard.release();
				s.obj_ = lptr;
				break;
			}
			default: {
				s = rs;
			}
			}

			kind(rk);
		}

	public:
		constexpr ~basic_json() noexcept
		{
			destroy();
		}

		constexpr slice_type slice()
		{
			return *this;
		}

		constexpr const_slice_type slice() const
		{
			return *this;
		}

		/* to maintain consistency with object, this implementation is not used
		template <typename... Args>
		    requires(std::constructible_from<basic_json, Args> && ...)
		static constexpr basic_json array(Args&&... args)
		{
		    array_type arr;
		    arr.reserve(sizeof...(Args));
		    (arr.push_back(basic_json(std::forward<Args>(args))), ...);

		    return arr;
		}
		*/
	private:
		struct pair
		{
			key_string_type key;
			basic_json value;
		};

		template <typename... Ts>
		struct type_list
		{
		};

		template <typename T1, typename T2, typename... Ts>
		static consteval std::size_t get_pair_num_pair(type_list<T1, T2, Ts...>) noexcept
		{
			return 1 + get_pair_num(type_list<Ts...>{});
		}

		template <typename T1, typename... Ts>
		static consteval std::size_t get_pair_num(type_list<T1, Ts...>) noexcept
		{
			if constexpr (std::same_as<map_node_type, std::remove_cvref_t<T1>>)
			{
				return 1 + get_pair_num(type_list<Ts...>{});
			}
			else
			{
				static_assert(sizeof...(Ts) > 0, "there should be a json value after string in json object");
				return get_pair_num_pair(type_list<T1, Ts...>{});
			}
		}

		static consteval std::size_t get_pair_num(type_list<>) noexcept
		{
			return 0;
		}

	public:
		template <std::size_t N>
		struct object
		{
			std::array<pair, N> arr;

			constexpr operator basic_json() &&
			{
				object_type obj;
				for (auto& pair : arr)
				{
					obj.emplace(std::move(pair.key), std::move(pair.value));
				}

				return obj;
			}
		};

		// deduction guide for object{...}
		template <typename... Ts>
		object(Ts&&...) -> object<get_pair_num(type_list<Ts...>{})>;

		struct array
		{
		private:
			basic_json json;

		public:
			template <typename... Ts>
			array(Ts&&... ts)
			    requires(std::constructible_from<basic_json, Ts> && ...)
			{
				array_type arr;
				arr.reserve(sizeof...(Ts));
				(arr.push_back(basic_json(std::forward<Ts>(ts))), ...);
				json = std::move(arr);
			}

			constexpr operator basic_json() &&
			{
				return std::move(json);
			}
		};
	};

	enum class json_option : unsigned int
	{
		// for deserializer
		allow_null = 0x0,
		allow_comment = 0x1,
		allow_overflow_number = 0x2,
		allow_underflow_number = 0x4,
		allow_overflow_integer = 0x8,
		allow_underflow_integer = 0x10,
		allow_overflow_uinteger = 0x20,
		treat_overflow_integer_as_number = 0x40,
		// for reflector
		treat_null_as_defaulted = 0x80,
		treat_undefined_as_defaulted = 0x100,
		// for serializer
		treat_empty_as_null = 0x200,
		treat_empty_as_undefined = 0x400,
		treat_empty_as_literal = 0x800, // debug only
	};

	enum class json_error
	{
		// for accessor
		is_empty = 1,
		not_null,
		not_boolean,
		not_number,
		not_integer,
		not_uinteger,
		not_array,
		not_object,
		// for modifier
		not_empty_or_null,
		not_empty_or_boolean,
		not_empty_or_number,
		not_empty_or_integer,
		not_empty_or_uinteger,
		not_empty_or_array,
		not_empty_or_object,
		// for deserializer
		unsupported_option,
		unexpect_token,
		unexpect_comment,
		unexpect_undefined,
		missing_square_bracket,
		missing_brace,
		missing_double_quote,
		number_overflow,
		number_underflow,
		integer_overflow,
		integer_underflow,
		uinteger_overflow,
		illegal_character,
		// for serializer
		unexpect_empty,
		number_nan,
		number_inf,
		// for reflector
		unexpect_null,
		unexpect_type,
	};

	using json = basic_json<>;
	using json_slice = basic_json_slice<>;
	using const_json_slice = basic_const_json_slice<>;
} // namespace bizwen
