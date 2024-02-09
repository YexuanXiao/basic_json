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
	// since the types of string, array and map are unknown at this point,
	// memory allocation can only be done by instantiating void.
	// This requires allocator<void>, allocator<string>, allocator<map>, and allocator<array>
	// satisfy DefaultConstructable and TrivialCopyable.
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

		union stor_t
		{
			void* str_;
			void* arr_;
			void* obj_;
			number_type num_;
			integer_type int_;
			uinteger_type uint_;
		};

		stor_t stor_;
		kind_t kind_;

	public:
		constexpr json_node() noexcept = default;
		constexpr json_node(json_node const&) = default;
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

		// spans need to store HasInteger and HasUInteger for deserializers
		static inline constexpr bool has_integer = HasInteger;
		static inline constexpr bool has_uinteger = HasUInteger;

	private:
		using json_t = basic_json<node_type, string_type, array_type, object_type, HasInteger, HasUInteger>;
		using kind_t = json_t::kind_t;
		using number_t = node_type::number_type;
		using boolean_t = node_type::boolean_type;
		using integer_t = node_type::integer_type;
		using uinteger_t = node_type::uinteger_type;
		using char_t = string_type::value_type;
		using key_string_t = object_type::key_type;
		using key_char_t = key_string_t::value_type;
		using stor_t = json_t::stor_t;

		friend class basic_json<node_type, string_type, array_type, object_type, HasInteger, HasUInteger>;

		json_t* json_{};

		constexpr kind_t kind() const noexcept
		{
			assert(json_);

			return json_->kind_;
		}

		constexpr stor_t const& stor() const noexcept
		{
			return json_->stor_;
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

		// similar to iterators, default construction is allowed, but except for operator=,
		// operations on default-constructed span cause undefined behavior.
		constexpr basic_const_json_span() noexcept = default;

		constexpr basic_const_json_span(basic_const_json_span&& rhs) noexcept = default;

		constexpr basic_const_json_span(basic_const_json_span const& rhs) noexcept = default;

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

		constexpr basic_const_json_span& operator=(basic_const_json_span const& rhs) noexcept = default;

		constexpr basic_const_json_span& operator=(basic_const_json_span&& rhs) noexcept = default;

		constexpr explicit operator boolean_t() const
		{
			if (!boolean())
				throw std::runtime_error("json error: value not a boolean.");

			auto k = kind();

			return k == kind_t::true_value ? 1 : 0;
		}

		constexpr explicit operator number_t() const
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

		constexpr explicit operator const string_type&() const&
		{
			if (!string())
				throw std::runtime_error("json error: value isn't a string.");

			return *static_cast<string_type const*>(json_->stor_.str_);
		}

		constexpr explicit operator const array_type&() const&
		{
			if (!array())
				throw std::runtime_error("json error: value isn't an array.");

			return *static_cast<array_type const*>(json_->stor_.arr_);
		}

		constexpr explicit operator const object_type&() const&
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object.");

			return *static_cast<object_type const*>(json_->stor_.obj_);
		}

		constexpr explicit operator integer_t() const
		    requires(HasInteger)
		{
			if (!integer())
				throw std::runtime_error("json error: value isn't an integer.");

			return json_->stor_.int_;
		}

		constexpr explicit operator uinteger_t() const
		    requires(HasUInteger)
		{
			if (!uinteger())
				throw std::runtime_error("json error: value isn't an unsigned integer.");

			return json_->stor_.uint_;
		}

		constexpr basic_const_json_span operator[](key_string_t const& k) const
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			auto const& o = *static_cast<object_type const*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto const& [key, v] = *i;

			return v;
		}

		template <typename KeyStrLike>
		    requires std::convertible_to<KeyStrLike, key_string_t> || std::convertible_to<key_string_t, KeyStrLike>
		constexpr basic_const_json_span operator[](KeyStrLike const& k) const
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			auto const& o = *static_cast<object_type const*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto const& [_, v] = *i;

			return v;
		}

		constexpr basic_const_json_span operator[](key_char_t* k) const
		{
			if (!object())
				throw std::runtime_error("json error: value isn't an object but is accessed using operator[].");

			auto const& o = *static_cast<object_type const*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw std::runtime_error("key does not exist.");

			auto const& [_, v] = *i;

			return v;
		}

		constexpr basic_const_json_span operator[](array_type::size_type pos) const noexcept
		{
			if (!array())
				throw std::runtime_error("json error: value isn't an array but is accessed using operator[].");

			auto const& a = *static_cast<array_type const*>(stor().arr_);

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

		using stor_t = node_type::stor_t;
		friend class basic_const_json_span<node_type, string_type, array_type, object_type, HasInteger, HasUInteger>;

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

		constexpr kind_t kind() const noexcept
		{
			return (*this).kind_;
		}

		constexpr stor_t& stor() noexcept
		{
			return (*this).stor_;
		}

		constexpr stor_t const& stor() const noexcept
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
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(std::move(v));
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(char_t const* begin, char_t const* end)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(begin, end);
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(char_t const* str, string_type::size_type count)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(str, count);
			guard.release();
			kind(kind_t::string);
		}

		constexpr explicit basic_json(char_t const* str)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(str);
			guard.release();
			kind(kind_t::string);
		}

		template <typename StrLike>
		    requires std::convertible_to<StrLike, string_type> || std::convertible_to<string_type, StrLike>
		constexpr basic_json(StrLike str)
		{
			alloc_guard_<string_type> guard(*this);
			stor().str_ = new (guard.get()) string_type(str);
			guard.release();
			kind(kind_t::string);
		}

		constexpr explicit basic_json(array_type arr)
		{
			rollbacker_array_all_ rollbacker(arr);
			alloc_guard_<array_type> guard(*this);
			stor().arr_ = new (guard.get()) array_type(std::move(arr));
			guard.release();
			rollbacker.release();
			kind(kind_t::array);
		}

		constexpr explicit basic_json(object_type obj)
		{
			rollbacker_map_all_ rollbacker(obj);
			alloc_guard_<object_type> guard(*this);
			stor().arr_ = new (guard.get()) object_type(std::move(obj));
			guard.release();
			rollbacker.release();
			kind(kind_t::object);
		}

		constexpr explicit basic_json(node_type&& n) noexcept
		{
			this->kind_ = decltype(this->kind_){};
			this->stor_ = decltype(this->stor_){};
			static_cast<node_type&>(*this) = std::move(n);
		}

		/* private inherit prevent this cast operator
		constexpr operator node_type() && noexcept
		{
		    auto node = static_cast<node_type&>(*this);
		    kind(kind_t{});
		    stor() = decltype(stor()){};

		    return node;
		}
		*/

		[[nodiscard("discard nodes will cause leaks")]] constexpr node_type move_to_node() && noexcept
		{
			auto node = static_cast<node_type&>(*this);
			kind(kind_t{});
			stor() = std::remove_reference_t<decltype(stor())>{};

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
					static_cast<basic_json&>(*sentry).clone(static_cast<basic_json const&>(*first));
				}

				guard.release();
				s.arr_ = lptr;
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
					temp.clone(static_cast<basic_json const&>(rvalue));
					lobj.emplace(rkey, node_type(std::move(temp)));
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
		constexpr basic_json(const basic_json& rhs)
		{
			clone(rhs);
		}

		constexpr basic_json& operator=(const basic_json& rhs)
		{
			destroy();
			clone(rhs);

			return *this;
		}

		constexpr ~basic_json() noexcept
		{
			destroy();
		}
	};

	using json = basic_json<>;
	using const_json_span = basic_const_json_span<>;

} // namespace bizwen
