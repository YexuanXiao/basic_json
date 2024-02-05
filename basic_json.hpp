#include <cassert>
#include <iterator>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>
#include <concepts>

// https://www.rfc-editor.org/rfc/rfc8259

namespace bizwen
{
	class nulljson_t
	{
	public:
		explicit constexpr nulljson_t(decltype(nullptr)) {}
	};

	inline constexpr nulljson_t nulljson{ nullptr };

	// https://cplusplus.github.io/LWG/issue3917
	// since the types of string, array and map are not known at this point
	// memory allocation can only be done by instantiating void
	// requires allocator<void>, allocator<string>, allocator<map>,
	// and allocator<array> satisfy DefaultConstructable and TrivialCopyable
	template <typename Boolean = bool, typename Number = double,
	    typename Integer = long long, typename UInteger = unsigned long long, typename Allocator = std::allocator<void>>
	class json_node: protected Allocator
	{
	public:
		using number_type = Number;
		using boolean_type = Boolean;
		using integer_type = Integer;
		using uinteger_type = UInteger;
		using allocator_type = Allocator;

		static_assert(std::integral<boolean_type>);
		static_assert(std::floating_point<number_type>);
		static_assert(std::signed_integral<integer_type>);
		static_assert(std::unsigned_integral<uinteger_type>);
		static_assert(std::same_as<void, typename allocator_type::value_type>);

	protected:
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

		union stor_t_
		{
			void* str_;
			void* arr_;
			void* obj_;
			number_type num_;
			integer_type int_;
			uinteger_type uint_;
		};

		stor_t_ stor_;
		kind_t kind_;

	public:
		constexpr json_node() noexcept = default;

		constexpr json_node(json_node const&) = delete;

		constexpr json_node(json_node&& rhs) noexcept = default;
		constexpr json_node& operator=(json_node&& rhs) noexcept = default;
	};

	template <typename Node = json_node<>, typename String = std::string,
	    typename Array = std::vector<Node>,
	    typename Map = std::map<String, Node>,
	    bool HasInteger = true, bool HasUInteger = true>
	class basic_json;

	template <typename Node = json_node<>, typename String = std::string,
	    typename Array = std::vector<Node>,
	    typename Map = std::map<String, Node>,
	    bool HasInteger = true, bool HasUInteger = true>
	class basic_const_json_span
	{
		using node_type = Node;
		using value_type = Node;
		using object_type = Map;
		using array_type = Array;
		using string_type = String;

		// span need to store HasInteger and has uinteger for deserializers
		static inline constexpr bool has_integer = HasInteger;
		static inline constexpr bool has_uinteger = HasUInteger;

	private:
		using kind_t = node_type::kind_t;
		using number_t = node_type::number_t;
		using boolean_t = node_type::boolean_t;
		using integer_t = node_type::integer_t;
		using uinteger_t = node_type::uinteger_t;
		using char_t = string_type::value_type;
		using key_string_t = object_type::key_type;
		using key_char_t = key_string_t::value_type;

		using json_t = basic_json<node_type, string_type, array_type, object_type, HasInteger, HasUInteger>;

		json_t* json_{};

		[[nodiscard]] constexpr kind_t kind() const noexcept
		{
			return json_->node_.kind_;
		}

		[[nodiscard]] constexpr node_type::stor_t_& stor() noexcept
		{
			return json_->node_.stor_;
		}

		[[nodiscard]] constexpr node_type::stor_t_ const& stor() const noexcept
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

			return k == kind_t::true_value || kind_t::false_value;
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
		    requires(HasInteger)
		{
			return kind() == kind_t::integer;
		}

		[[nodiscard]] constexpr bool uinteger() const noexcept
		    requires(HasUInteger)
		{
			return kind() == kind_t::uinteger;
		}

		constexpr void swap(basic_const_json_span& rhs) noexcept
		{
			auto temp = json_;
			json_ = rhs.json_;
			rhs.json_ = temp;
		}

		friend constexpr void swap(basic_const_json_span& lhs, basic_const_json_span& rhs) noexcept
		{
			lhs.swap(rhs);
		}

		constexpr basic_const_json_span() noexcept = default;

		constexpr basic_const_json_span(basic_const_json_span&& rhs) noexcept = default;

		constexpr basic_const_json_span(json_t& j) noexcept
		    : json_(&j)
		{
		}

		constexpr basic_const_json_span(json_t const& j) noexcept
		    : json_(&j)
		{
		}

		// the cast has undefined behavior because it's derive-to-base
		basic_const_json_span(node_type const& n) noexcept
		    : json_(reinterpret_cast<json_t*>(const_cast<node_type*>(&n)))
		{
		}

		constexpr explicit operator boolean_t() const noexcept
		{
			if (!boolean())
				throw std::runtime_error("json_error: value not a boolean.");

			auto k = kind();

			return k == kind_t::true_value ? 1 : 0;
		}

		constexpr explicit operator number_t() const noexcept
		{
			auto k = kind();
			auto s = stor();

			if (k == kind_t::number)
				return s.num_;
			else if (k == kind_t::integer)
				return s.int_;
			else if (k == kind_t::uinteger)
				return s.uint_;

			throw std::runtime_error("json_error:value isn't a number .");
		}

		constexpr explicit operator nulljson_t() const noexcept
		{
			if (!null())
				throw std::runtime_error("json_error: value isn't a null.");

			return nulljson;
		}

		constexpr explicit operator const string_type&() const& noexcept
		{
			if (!string())
				throw std::runtime_error("json_error: value isn't a string.");

			return *(json_->stor_.str_);
		}

		constexpr explicit operator const array_type&() const& noexcept
		{
			if (!array())
				throw std::runtime_error("json_error: value isn't an array.");

			return *(json_->stor_.arr_);
		}

		constexpr explicit operator const object_type&() const& noexcept
		{
			if (!object())
				throw std::runtime_error("json_error: value isn't an object.");

			return *(json_->stor_.obj_);
		}

		constexpr explicit operator integer_t() const noexcept
		    requires(HasInteger)
		{
			if (!integer())
				throw std::runtime_error("json_error: value isn't an integer.");

			return json_->stor_.int_;
		}

		constexpr explicit operator uinteger_t() const noexcept
		    requires(HasUInteger)
		{
			if (!uinteger())
				throw std::runtime_error("json_error: value isn't an unsigned integer.");

			return json_->stor_.uint_;
		}

		constexpr basic_const_json_span operator[](key_string_t const& k)
		{
			if (!object())
				throw std::runtime_error("json_error: value isn't an object but is accessed using operator[].");

			auto& o = *stor().obj_;
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto&& [key, v] = *i;

			return v;
		}

		template <std::convertible_to<key_string_t> KeyStrLike>
		constexpr basic_const_json_span operator[](KeyStrLike const& k)
		{
			if (!object())
				throw std::runtime_error("json_error: value isn't an object but is accessed using operator[].");

			auto& o = *stor().obj_;
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto&& [_, v] = *i;

			return v;
		}

		constexpr basic_const_json_span operator[](key_char_t* k) const
		{
			if (!object())
				throw std::runtime_error("json_error: value isn't an object but is accessed using operator[].");

			auto& o = *stor().obj_;
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto&& [_, v] = *i;

			return v;
		}

		constexpr basic_const_json_span operator[](array_type::size_type pos) const noexcept
		{
			if (!array())
				throw std::runtime_error("json_error: value isn't an array but is accessed using operator[].");

			auto& a = *stor().arr_;

			return a[pos];
		}
	};

	template <typename Node, typename String,
	    typename Array,
	    typename Map,
	    bool HasInteger, bool HasUInteger>
	class basic_json: private Node
	{
		using node_type = Node;
		using object_type = Map;
		using value_type = Node;
		using array_type = Array;
		using string_type = String;

	private:
		using kind_t = node_type::kind_t;
		using number_t = node_type::number_type;
		using boolean_t = node_type::boolean_type;
		using integer_t = node_type::integer_type;
		using uinteger_t = node_type::uinteger_type;
		using char_t = string_type::value_type;
		using key_string_t = object_type::key_type;
		using key_char_t = key_string_t::value_type;

		// json needs to aware the allocator, but span does not
		using allocator_t = node_type::allocator_type;
		using traits_t = std::allocator_traits<allocator_t>;

		static_assert(std::integral<char_t>);
		static_assert(std::integral<boolean_t>);
		static_assert(std::integral<key_char_t>);
		static_assert(std::floating_point<number_t>);
		static_assert(std::signed_integral<integer_t>);
		static_assert(std::unsigned_integral<uinteger_t>);
		static_assert(sizeof(boolean_t) < sizeof(integer_t));
		static_assert(sizeof(integer_t) == sizeof(uinteger_t));
		static_assert(std::same_as<node_type, typename array_type::value_type>);
		static_assert(std::same_as<node_type, typename object_type::mapped_type>);
		static_assert(std::random_access_iterator<typename array_type::iterator>);
		static_assert(std::random_access_iterator<typename string_type::iterator>);
		static_assert(std::bidirectional_iterator<typename object_type::iterator>);
		static_assert(std::random_access_iterator<typename key_string_t::iterator>);
		static_assert(std::same_as<json_node<boolean_t, number_t, integer_t, uinteger_t>, node_type>);

		constexpr void kind(kind_t k) noexcept { (*this).kind_ = k; }

		[[nodiscard]] constexpr kind_t kind() const noexcept
		{
			return (*this).kind_;
		}

		[[nodiscard]] constexpr node_type::stor_t_& stor() noexcept
		{
			return (*this).stor_;
		}

		[[nodiscard]] constexpr node_type::stor_t_ const& stor() const noexcept
		{
			return (*this).stor_;
		}

		template <typename T>
		constexpr T* alloc()
		{
			return typename traits_t::template rebind_alloc<T>(*this).allocate(1);
		}

		template <typename T>
		constexpr void dealloc(T* p) noexcept
		{
			return typename traits_t::template rebind_alloc<T>(*this).deallocate(p, 1);
		}

		void dealloc() noexcept
		{
			auto k = kind();
			auto& s = stor();

			switch (k)
			{
			case kind_t::string: {
				dealloc(static_cast<string_type*>(s.str_));

				break;
			}
			case kind_t::array: {
				auto p = static_cast<array_type*>(s.arr_);

				for (auto&& i : *p)
				{
					auto&& json = basic_json(std::move(i));
					json.dealloc();
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
					json.dealloc();
				}

				(*p).~object_type();
				dealloc(p);

				break;
			}
			}
		}

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
				(*this).kind_ = rhs.kind();
				(rhs).kind_ = temp;
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

		constexpr basic_json(decltype(nullptr)) noexcept = delete; // prevent implicit construct string

		constexpr basic_json(boolean_t v) noexcept
		{
			v ? kind(kind_t::true_value) : kind(kind_t::false_value);
		}

		constexpr basic_json(bool v) noexcept
		    requires(!std::same_as<bool, boolean_t>)
		{
			v ? kind(kind_t::true_value) : kind(kind_t::false_value);
		}

		constexpr basic_json(number_t v) noexcept
		{
			stor().num_ = v;
			kind(kind_t::number);
		}

		template <std::floating_point T>
		constexpr basic_json(T v) noexcept
		{
			stor().num_ = v;
			kind(kind_t::number);
		}

		template <std::signed_integral T>
		    requires(sizeof(T) > sizeof(boolean_t)) && HasInteger
		constexpr basic_json(T v) noexcept
		{
			stor().int_ = v;
			kind(kind_t::integer);
		}

		template <std::unsigned_integral T>
		    requires(sizeof(T) > sizeof(boolean_t)) && HasUInteger
		constexpr basic_json(T v) noexcept
		{
			stor().uint_ = v;
			kind(kind_t::uinteger);
		}

		constexpr basic_json(integer_t v) noexcept
		    requires HasInteger
		{
			stor().int_ = v;
			kind(kind_t::integer);
		}

		constexpr basic_json(uinteger_t v) noexcept
		    requires HasUInteger
		{
			stor().uint_ = v;
			kind(kind_t::integer);
		}

		constexpr explicit basic_json(string_type v)
		{
			stor().str_ = new (alloc<string_type>()) string_type(std::move(v));
			kind(kind_t::string);
		}

		constexpr basic_json(char_t const* begin, char_t const* end)
		{
			stor().str_ = new (alloc<string_type>()) string_type(begin, end);
			kind(kind_t::string);
		}

		constexpr basic_json(char_t const* str, string_type::size_type count)
		{
			stor().str_ = new (alloc<string_type>()) string_type(str, count);
			kind(kind_t::string);
		}

		constexpr explicit basic_json(char_t const* str)
		{
			stor().str_ = new (alloc<string_type>()) string_type(str);
			kind(kind_t::string);
		}

		template <std::convertible_to<string_type> StrLike>
		constexpr basic_json(StrLike str)
		{
			stor().str_ = new (alloc<string_type>()) string_type(str);
			kind(kind_t::string);
		}

		constexpr explicit basic_json(array_type arr)
		{
			stor().arr_ = new (alloc<array_type>()) array_type(arr);
			kind(kind_t::array);
		}

		constexpr explicit basic_json(object_type obj)
		{
			stor().arr_ = new (alloc<object_type>()) object_type(obj);
			kind(kind_t::obj);
		}

		constexpr explicit basic_json(node_type&& n) noexcept
		{
			this->kind_ = decltype(this->kind_){};
			this->stor_ = decltype(this->stor_){};
			static_cast<node_type&>(*this) = std::move(n);
		}

		constexpr operator node_type() && noexcept
		{
			auto node = static_cast<node_type&>(*this);
			kind(kind_t{});
			stor() = decltype(stor()){};

			return node;
		}

		constexpr ~basic_json() noexcept
		{
			dealloc();
		}
	};

	using json = basic_json<>;

} // namespace bizwen
