#ifndef BIZWEN_BASIC_JSON_HPP
#define BIZWEN_BASIC_JSON_HPP

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

	namespace detail
	{
		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		struct basic_json_slice_common_base;

		template <typename T, typename Node>
		constexpr T* alloc_from_node(Node& node);

		template <typename T, typename Node>
		constexpr void dealloc_from_node(Node& node, T* p) noexcept;

		template <typename T, typename Node>
		struct node_variant_alloc_guard;
	} // namespace detail

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

	enum class json_errc
	{
		// for accessor
		is_empty = 1,
		not_null,
		not_boolean,
		not_number,
		not_integer,
		not_uinteger,
		not_string,
		not_array,
		not_object,
		nonarray_indexing,
		nonobject_indexing,
		key_not_found,
		// for modifier
		not_empty_or_null,
		not_empty_or_boolean,
		not_empty_or_number,
		not_empty_or_integer,
		not_empty_or_uinteger,
		not_empty_or_string,
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
		too_deep,
		too_large,
		// for serializer
		unexpect_empty,
		number_nan,
		number_inf,
		// for reflector
		unexpect_null,
		unexpect_type,
	};

	class json_error: public std::runtime_error
	{
	public:
		json_error(json_errc ec)
		    : std::runtime_error("")
		    , code_(ec)
		{
		}

		json_errc code() const noexcept { return code_; }

		const char* what() const override
		{
			switch (code_)
			{
			case json_errc::not_null:
				return "JSON error: value isn't a null.";
			case json_errc::not_boolean:
				return "JSON error: value not a boolean.";
			case json_errc::not_integer:
				return "JSON error: value isn't an integer.";
			case json_errc::not_uinteger:
				return "JSON error: value isn't an unsigned integer.";
			case json_errc::not_number:
				return "JSON error: value isn't a number.";
			case json_errc::not_string:
				return "JSON error: value isn't a string.";
			case json_errc::not_array:
				return "JSON error: value isn't an array.";
			case json_errc::not_object:
				return "JSON error: value isn't an object.";
			case json_errc::nonarray_indexing:
				return "JSON error: value isn't an array but is accessed using operator[].";
			case json_errc::nonobject_indexing:
				return "JSON error: value isn't an object but is accessed using operator[].";
			case json_errc::key_not_found:
				return "JSON error: key does not exist.";
			case json_errc::not_empty_or_null:
				return "JSON error: current value is not empty or not null.";
			case json_errc::not_empty_or_boolean:
				return "JSON error: current value is not empty or not bool.";
			case json_errc::not_empty_or_number:
				return "JSON error: current value is not empty or not a number.";
			case json_errc::not_empty_or_string:
				return "JSON error: current value is not empty or not a string.";
			default:
				return "JSON error: unexpected error.";
			}
		}

	private:
		json_errc code_;
	};

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
		friend struct detail::basic_json_slice_common_base;

		template <typename T, typename Node>
		friend constexpr T* detail::alloc_from_node(Node& node);

		template <typename T, typename Node>
		friend constexpr void detail::dealloc_from_node(Node& node, T* p) noexcept;

		template <typename T, typename Node>
		friend struct detail::node_variant_alloc_guard;

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

		constexpr Allocator& get_allocator_ref() noexcept
		{
			return (Allocator&)(*this);
		}
		constexpr const Allocator& get_allocator_ref() const noexcept
		{
			return (const Allocator&)(*this);
		}

	public:
		constexpr basic_json_node() noexcept(std::is_nothrow_default_constructible_v<Allocator>)
		    requires std::default_initializable<Allocator>
		    : Allocator()
		{
		}

		constexpr explicit basic_json_node(Allocator const& a) noexcept
		    : Allocator(a)
		{
		}

		constexpr basic_json_node(basic_json_node const&) = default;
		constexpr basic_json_node(basic_json_node&&) noexcept = default;
		constexpr basic_json_node& operator=(basic_json_node const&) = default;
		constexpr basic_json_node& operator=(basic_json_node&&) noexcept = default;
	};

	namespace detail
	{
		template <typename Object>
		concept transparently_comparable_associative = requires {
			typename Object::key_compare;
			typename Object::key_compare::is_transparent;
		};

		template <typename Object>
		concept transparently_comparable_unordered_associative = requires {
			typename Object::key_equal;
			typename Object::key_equal::is_transparent;
			typename Object::hasher;
			typename Object::hasher::is_transparent;
		};

		template <typename Object>
		concept transparently_comparable_json_object = transparently_comparable_associative<Object>
		    || transparently_comparable_unordered_associative<Object>;

		template <typename KeyStrLike, typename Object>
		concept noncovertible_to_key_char_cptr = !std::is_convertible_v<KeyStrLike const&, typename Object::key_type::value_type const*>;

		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		struct basic_json_slice_common_base
		{
			static inline constexpr bool has_integer = HasInteger;
			static inline constexpr bool has_uinteger = HasUInteger;

			using node_type = Node;
			using object_type = Object;
			using value_type = Node;
			using array_type = Array;
			using string_type = String;

			using const_slice_type = basic_const_json_slice<node_type, string_type, array_type, object_type, has_integer, has_uinteger>;
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

			node_type* node_{}; // made private in derived classes

		private:
			using traits_t = std::allocator_traits<allocator_type>;
			using kind_t = node_type::kind_t;
			using stor_t = node_type::stor_t;

			friend json_type;

			constexpr kind_t kind() const noexcept
			{
				assert(node_);

				return node_->kind_;
			}

			constexpr stor_t const& stor() const noexcept
			{
				return node_->stor_;
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

			constexpr explicit operator bool() const
			{
				if (!boolean())
					throw json_error(json_errc::not_boolean);

				auto k = kind();

				return k == kind_t::true_value;
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

				throw json_error(json_errc::not_number);
			}

			constexpr explicit operator nulljson_t() const
			{
				if (!null())
					throw json_error(json_errc::not_null);

				return nulljson;
			}

			constexpr explicit operator string_type const&() const&
			{
				if (!string())
					throw json_error(json_errc::not_string);

				return *static_cast<string_type const*>(stor().str_);
			}

			constexpr explicit operator array_type const&() const&
			{
				if (!array())
					throw json_error(json_errc::not_array);

				return *static_cast<array_type const*>(stor().arr_);
			}

			constexpr explicit operator object_type const&() const&
			{
				if (!object())
					throw json_error(json_errc::not_object);

				return *static_cast<object_type const*>(stor().obj_);
			}

			constexpr explicit operator integer_type() const
			    requires HasInteger
			{
				if (!integer())
					throw json_error(json_errc::not_integer);

				return stor().int_;
			}

			constexpr explicit operator uinteger_type() const
			    requires HasUInteger
			{
				if (!uinteger())
					throw json_error(json_errc::not_uinteger);

				return stor().uint_;
			}

			// defined after the class body of basic_const_json_slice

			constexpr auto operator[](key_string_type const& k) const -> const_slice_type;

			template <typename KeyStrLike>
			    requires transparently_comparable_json_object<Object>
			    && noncovertible_to_key_char_cptr<KeyStrLike, Object>
			constexpr auto operator[](KeyStrLike const& k) const -> const_slice_type;

			constexpr auto operator[](key_char_type const* k) const -> const_slice_type;

			constexpr auto operator[](array_type::size_type pos) const -> const_slice_type;

			constexpr auto as_array() const
			{
				constexpr auto node_to_slice = [](node_type const& node) static noexcept {
					return const_slice_type{ node };
				};
				return static_cast<array_type const&>(*this) | std::views::transform(node_to_slice);
			}

			constexpr auto as_object() const
			{
				constexpr auto pair_node_to_slice = [](map_node_type const& pair) static noexcept {
					auto& [key, value]{ pair };
					return std::pair<key_string_type const&, const_slice_type>{ key, value };
				};
				return static_cast<object_type const&>(*this) | std::views::transform(pair_node_to_slice);
			}
		};
	}

	template <typename Node, typename String,
	    typename Array,
	    typename Object,
	    bool HasInteger, bool HasUInteger>
	class basic_const_json_slice: public detail::basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>
	{
	private:
		using base_type = detail::basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>;
		using base_type::node_;

	public:
		using typename base_type::json_type;
		using typename base_type::node_type;
		using typename base_type::slice_type;

		constexpr void swap(basic_const_json_slice& rhs) noexcept
		{
			auto temp = node_;
			node_ = rhs.node_;
			rhs.node_ = temp;
		}

		friend constexpr void swap(basic_const_json_slice& lhs, basic_const_json_slice& rhs) noexcept
		{
			lhs.swap(rhs);
		}

		// similar to iterators, default construction is allowed, but except for operator=,
		// operations on default-constructed slice cause undefined behavior.
		constexpr basic_const_json_slice() noexcept = default;

		constexpr basic_const_json_slice(basic_const_json_slice&&) noexcept = default;

		constexpr basic_const_json_slice(basic_const_json_slice const&) noexcept = default;

		constexpr basic_const_json_slice(json_type const& j) noexcept
		    : base_type{ const_cast<node_type*>(std::addressof(j.node_)) }
		{
		}

		constexpr basic_const_json_slice(node_type const& n) noexcept
		    : base_type{ const_cast<node_type*>(std::addressof(n)) }
		{
		}

		constexpr basic_const_json_slice(slice_type const& s) noexcept
		    : base_type{ static_cast<base_type const&>(s) }
		{
		}

		constexpr basic_const_json_slice& operator=(basic_const_json_slice const&) noexcept = default;

		constexpr basic_const_json_slice& operator=(basic_const_json_slice&&) noexcept = default;
	};

	namespace detail
	{
		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		constexpr auto basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>::operator[](key_string_type const& k) const -> const_slice_type
		{
			if (!object())
				throw json_error(json_errc::nonobject_indexing);

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw json_error(json_errc::key_not_found);

			auto& [_, v] = *i;

			return v;
		}

		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		template <typename KeyStrLike>
		    requires transparently_comparable_json_object<Object>
		    && noncovertible_to_key_char_cptr<KeyStrLike, Object>
		constexpr auto basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>::operator[](KeyStrLike const& k) const -> const_slice_type
		{
			if (!object())
				throw json_error(json_errc::nonobject_indexing);

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw json_error(json_errc::key_not_found);

			auto& [_, v] = *i;

			return v;
		}

		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		constexpr auto basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>::operator[](key_char_type const* k) const -> const_slice_type
		{
			if (!object())
				throw json_error(json_errc::nonobject_indexing);

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto i = o.find(k);

			if (i == o.end())
				throw json_error(json_errc::key_not_found);

			auto& [_, v] = *i;

			return v;
		}

		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		constexpr auto basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>::operator[](array_type::size_type pos) const -> const_slice_type
		{
			if (!array())
				throw json_error(json_errc::nonarray_indexing);

			auto& a = *static_cast<array_type*>(stor().arr_);

			return a[pos];
		}

		template <typename T, typename Node>
		constexpr T* alloc_from_node(Node& node)
		{
			using ator_t = std::allocator_traits<typename Node::allocator_type>::template rebind_alloc<T>;
			return std::to_address(ator_t(node.get_allocator_ref()).allocate(1));
		}

		template <typename T, typename Node>
		constexpr void dealloc_from_node(Node& node, T* p) noexcept
		{
			using ator_t = std::allocator_traits<typename Node::allocator_type>::template rebind_alloc<T>;
			using ator_ptr = std::allocator_traits<ator_t>::pointer;
			using ator_ptr_traits = std::pointer_traits<ator_ptr>;
			return ator_t(node.get_allocator_ref()).deallocate(ator_ptr_traits::pointer_to(*p), 1);
		}

		template <typename T, typename Node>
		struct node_variant_alloc_guard
		{
			T* ptr;
			Node& node;

			node_variant_alloc_guard(node_variant_alloc_guard const&) = delete;

			constexpr explicit node_variant_alloc_guard(Node& n)
			    : node(n)
			{
				ptr = detail::alloc_from_node<T>(node);
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

			constexpr ~node_variant_alloc_guard()
			{
				if (ptr)
					detail::dealloc_from_node(node, ptr);
			}
		};
	}

	template <typename Node, typename String,
	    typename Array,
	    typename Object,
	    bool HasInteger, bool HasUInteger>
	class basic_json_slice: public detail::basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>
	{
	private:
		using base_type = detail::basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>;
		using base_type::node_;

	public:
		using typename base_type::allocator_type;
		using typename base_type::array_type;
		using typename base_type::char_type;
		using typename base_type::json_type;
		using typename base_type::node_type;
		using typename base_type::object_type;
		using typename base_type::string_type;

		using typename base_type::key_char_type;
		using typename base_type::key_string_type;
		using typename base_type::map_node_type;

		using base_type::array;
		using base_type::boolean;
		using base_type::empty;
		using base_type::integer;
		using base_type::null;
		using base_type::number;
		using base_type::object;
		using base_type::string;
		using base_type::uinteger;

		constexpr void swap(basic_json_slice& rhs) noexcept
		{
			auto temp = node_;
			node_ = rhs.node_;
			rhs.node_ = temp;
		}

		friend constexpr void swap(basic_json_slice& lhs, basic_json_slice& rhs) noexcept
		{
			lhs.swap(rhs);
		}

		// similar to iterators, default construction is allowed, but except for operator=,
		// operations on default-constructed slice cause undefined behavior.
		constexpr basic_json_slice() noexcept = default;

		constexpr basic_json_slice(basic_json_slice&&) noexcept = default;

		constexpr basic_json_slice(basic_json_slice const&) noexcept = default;

		constexpr basic_json_slice(json_type& j) noexcept
		    : base_type{ std::addressof(j.node_) }
		{
		}

		constexpr basic_json_slice(node_type& n) noexcept
		    : base_type{ std::addressof(n) }
		{
		}

		constexpr basic_json_slice& operator=(basic_json_slice const&) noexcept = default;

		constexpr basic_json_slice& operator=(basic_json_slice&&) noexcept = default;

		constexpr explicit operator string_type&() &
		{
			if (!string())
				throw json_error(json_errc::not_string);

			return *static_cast<string_type*>(stor().str_);
		}

		constexpr explicit operator array_type&() &
		{
			if (!array())
				throw json_error(json_errc::not_array);

			return *static_cast<array_type*>(stor().arr_);
		}

		constexpr explicit operator object_type&() &
		{
			if (!object())
				throw json_error(json_errc::not_object);

			return *static_cast<object_type*>(stor().obj_);
		}

	private:
		using kind_t = node_type::kind_t;
		using stor_t = node_type::stor_t;

		using string_ator = std::allocator_traits<allocator_type>::template rebind_alloc<String>;
		using string_ator_traits = std::allocator_traits<string_ator>;
		using object_ator = std::allocator_traits<allocator_type>::template rebind_alloc<Object>;
		using object_ator_traits = std::allocator_traits<object_ator>;

		constexpr void allocate_object()
		{
			detail::node_variant_alloc_guard<object_type, node_type> guard(*node_);
			auto ator = object_ator(node_->get_allocator_ref());
			auto rawptr = guard.get();
			object_ator_traits::construct(ator, rawptr);
			stor().obj_ = rawptr;
			guard.release();
			node_->kind_ = kind_t::object;
		}

		constexpr stor_t& stor() noexcept
		{
			return node_->stor_;
		}

	public:
		using base_type::operator[];

		constexpr basic_json_slice operator[](key_string_type const& k)
		{
			auto e = empty();

			if (!object() && !e)
				throw json_error(json::errc::nonobject_indexing);

			if (e)
				allocate_object();

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto [i, _] = o.emplace(k, node_type{ node_->get_allocator_ref() });
			auto& [_, v] = *i;

			return v;
		}

		template <typename KeyStrLike>
		    requires detail::transparently_comparable_json_object<Object>
		    && detail::noncovertible_to_key_char_cptr<KeyStrLike, Object>
		constexpr basic_json_slice operator[](KeyStrLike const& k)
		{
			auto e = empty();

			if (!object() && !e)
				throw json_error(json::errc::nonobject_indexing);

			if (e)
				allocate_object();

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto [i, _] = o.emplace(k, node_type{ node_->get_allocator_ref() });
			auto& [_, v] = *i;

			return v;
		}

		constexpr basic_json_slice operator[](key_char_type const* k)
		{
			auto e = empty();

			if (!object() && !e)
				throw json_error(json_errc::nonobject_indexing);

			if (e)
				allocate_object();

			auto& o = *static_cast<object_type*>(stor().obj_);
			auto [i, _] = o.emplace(k, node_type{ node_->get_allocator_ref() });
			auto& [_, v] = *i;

			return v;
		}

		constexpr basic_json_slice operator[](array_type::size_type pos)
		{
			if (!array())
				throw json_error(json_errc::nonarray_indexing);

			auto const& a = *static_cast<array_type const*>(stor().arr_);

			return a[pos];
		}

		constexpr basic_json_slice& operator=(string_type const& str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string && !is_empty)
				throw json_error(json_errc::not_empty_or_string);

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = str;
			}
			else // empty
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, rawptr, str);
				stor().str_ = rawptr;
				guard.release();
				node_->kind_ = kind_t::string;
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(string_type&& str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string && !is_empty)
				throw json_error(json_errc::not_empty_or_string);

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = std::move(str);
			}
			else // empty
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, rawptr, std::move(str));
				stor().str_ = rawptr;
				guard.release();
				node_->kind_ = kind_t::string;
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(char_type const* str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string && !is_empty)
				throw json_error(json_errc::not_empty_or_string);

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = str;
			}
			else // empty
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, rawptr, str);
				stor().str_ = rawptr;
				guard.release();
				node_->kind_ = kind_t::string;
			}

			return *this;
		}

		template <typename StrLike>
		    requires std::constructible_from<string_type, StrLike>
		    && (!std::is_convertible_v<StrLike const&, char_type const*>)
		constexpr basic_json_slice& operator=(StrLike const& str)
		{
			bool is_string = string();
			bool is_empty = empty();

			if (!is_string && !is_empty)
				throw json_error(json_errc::not_empty_or_string);

			if (is_string)
			{
				*static_cast<string_type*>(stor().str_) = str;
			}
			else
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, rawptr, str);
				stor().str_ = rawptr;
				guard.release();
				node_->kind_ = kind_t::string;
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(nulljson_t)
		{
			bool is_null = null();
			bool is_empty = empty();

			if (!is_null && !is_empty)
				throw json_error(json_errc::not_empty_or_null);

			if (is_empty)
				node_->kind_ = kind_t::null;

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
					throw json_error(json_errc::not_empty_or_boolean);

				if (is_empty)
					node_->kind_ = (n ? kind_t::true_value : kind_t::false_value);
			}
			else if constexpr (HasInteger && std::signed_integral<T>)
			{
				bool is_number = number() || uinteger() || integer();
				bool is_empty = empty();

				if (!is_number && !is_empty)
					throw json_error(json_errc::not_empty_or_number);

				node_->stor_.int_ = n;
				node_->kind_ = kind_t::integer;
			}
			else if constexpr (HasUInteger && std::unsigned_integral<T>)
			{
				bool is_number = number() || uinteger() || integer();
				bool is_empty = empty();

				if (!is_number && !is_empty)
					throw json_error(json_errc::not_empty_or_number);

				node_->stor_.uint_ = n;
				node_->kind_ = kind_t::uinteger;
			}
			else // fallback
			{
				bool is_number = number() || uinteger() || integer();
				bool is_empty = empty();

				if (!is_number && !is_empty)
					throw json_error(json_errc::not_empty_or_number);

				node_->stor_.num_ = n;
				node_->kind_ = kind_t::number;
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(json_type& j) noexcept
		{
			node_ = std::addressof(j.node_);

			return *this;
		}

		constexpr basic_json_slice& operator=(node_type& n) noexcept
		{
			node_ = std::addressof(n);

			return *this;
		}

		using base_type::as_array;

		constexpr auto as_array()
		{
			constexpr auto node_to_slice = [](node_type const& node) static noexcept {
				return basic_json_slice{ node };
			};
			return static_cast<array_type&>(*this) | std::views::transform(node_to_slice);
		}

		using base_type::as_object;

		constexpr auto as_object()
		{
			constexpr auto pair_node_to_slice = [](map_node_type const& pair) static noexcept {
				auto& [key, value]{ pair };
				return std::pair<key_string_type const&, basic_json_slice>{ key, value };
			};
			return static_cast<object_type&>(*this) | std::views::transform(pair_node_to_slice);
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

		using string_ator = std::allocator_traits<allocator_type>::template rebind_alloc<String>;
		using string_ator_traits = std::allocator_traits<string_ator>;
		using array_ator = std::allocator_traits<allocator_type>::template rebind_alloc<Array>;
		using array_ator_traits = std::allocator_traits<array_ator>;
		using object_ator = std::allocator_traits<allocator_type>::template rebind_alloc<Object>;
		using object_ator_traits = std::allocator_traits<object_ator>;

		static constexpr bool is_ator_stateless_ = std::allocator_traits<allocator_type>::is_always_equal::value;
		static constexpr bool is_pocca_ = std::allocator_traits<allocator_type>::propagate_on_container_copy_assignment::value;
		static constexpr bool is_pocma_ = std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value;
		static constexpr bool is_pocs_ = std::allocator_traits<allocator_type>::propagate_on_container_swap::value;

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
		static_assert(std::same_as<basic_json_node<number_type, integer_type, uinteger_type, allocator_type>, node_type>);

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

		static constexpr void clear_node(node_type& node) noexcept
		{
			auto& s = node.stor_;

			switch (node.kind_)
			{
			case kind_t::string: {
				auto p = static_cast<string_type*>(s.str_);
				auto ator = string_ator(node.get_allocator_ref());
				string_ator_traits::destroy(ator, p);
				detail::dealloc_from_node(node, p);

				break;
			}
			case kind_t::array: {
				auto p = static_cast<array_type*>(s.arr_);

				for (auto&& i : *p)
				{
					auto&& json = basic_json(std::move(i));
					json.clear();
				}

				auto ator = array_ator(node.get_allocator_ref());
				array_ator_traits::destroy(ator, p);
				detail::dealloc_from_node(node, p);

				break;
			}
			case kind_t::object: {
				auto p = static_cast<object_type*>(s.obj_);

				for (auto&& [_, v] : *p)
				{
					auto&& json = basic_json(std::move(v));
					json.clear();
				}

				auto ator = object_ator(node.get_allocator_ref());
				object_ator_traits::destroy(ator, p);
				detail::dealloc_from_node(node, p);

				break;
			}
			default: {
				// make clang happy
			}
			}

			s = stor_t{};
			node.kind_ = kind_t{};
		}

		constexpr void clear() noexcept
		{
			clear_node(node_);
		}

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
					basic_json::clear_node(*begin);
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
					basic_json::clear_node(*begin);
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
					basic_json::clear_node(value);
				}
			}
		};

		constexpr void swap_without_ator(basic_json& rhs) noexcept
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

	public:
		constexpr void swap(basic_json& rhs) noexcept // strengthened
		{
			swap_without_ator(rhs);
			if constexpr (!is_ator_stateless_)
			{
				if constexpr (is_pocs_)
				{
					using std::swap;
					swap(node_.get_allocator_ref(), rhs.node_.get_allocator_ref()); // ADL-swap
				}
				else
				{
					// UB if unequal
					assert(node_.get_allocator_ref() == rhs.node_.get_allocator_ref());
				}
			}
		}

		friend constexpr void swap(basic_json& lhs, basic_json& rhs) noexcept // strengthened
		{
			lhs.swap(rhs);
		}

		// clang-format off
		constexpr basic_json() requires std::default_initializable<allocator_type> = default;
		// clang-format on

		constexpr explicit basic_json(allocator_type const& a) noexcept
		    : node_(a)
		{
		}

		constexpr basic_json(basic_json&& rhs) noexcept
		    : node_(rhs.node_.get_allocator_ref())
		{
			rhs.swap_without_ator(*this);
		}
		constexpr basic_json(basic_json&& rhs, allocator_type const& a) noexcept(is_ator_stateless_)
		    : node_(a)
		{
			if constexpr (is_ator_stateless_)
			{
				rhs.swap_without_ator(*this);
			}
			else
			{
				if (a == rhs.node_.get_allocator_ref())
				{
					rhs.swap_without_ator(*this);
				}
				else
				{
					clone(rhs);
					rhs.clear();
				}
			}
		}

		constexpr basic_json(basic_json const& rhs)
		    : node_(traits_t::select_on_container_copy_construction(rhs.node_.get_allocator_ref()))
		{
			clone(rhs);
		}
		constexpr explicit basic_json(basic_json const& rhs, allocator_type const& a)
		    : node_(a)
		{
			clone(rhs);
		}

		constexpr basic_json& operator=(basic_json const& rhs)
		{
			if (this != std::addressof(rhs))
			{
				clear();
				if constexpr (!is_ator_stateless_ && is_pocca_)
				{
					node_.get_allocator_ref() = rhs.node_.get_allocator_ref();
				}
				clone(rhs);
			}

			return *this;
		}

		constexpr basic_json& operator=(basic_json&& rhs) noexcept(is_ator_stateless_ || is_pocma_)
		{
			if constexpr (is_ator_stateless_)
			{
				rhs.swap_without_ator(*this);
			}
			else if constexpr (is_pocma_)
			{
				rhs.swap_without_ator(*this);
				node_.get_allocator_ref() = std::move(rhs.node_.get_allocator_ref());
			}
			else
			{
				if (node_.get_allocator_ref() == rhs.node_.get_allocator_ref())
				{
					rhs.swap_without_ator(*this);
				}
				else
				{
					clear();
					clone(rhs);
					rhs.clear();
				}
			}

			return *this;
		}

		constexpr basic_json(decltype(nullptr), allocator_type const& = allocator_type()) noexcept = delete; // prevent implicit constructing string

		constexpr basic_json(nulljson_t, allocator_type const& a = allocator_type()) noexcept
		    : node_(a)
		{
			kind(kind_t::null);
		}

		template <typename T>
		    requires std::is_arithmetic_v<T>
		constexpr basic_json(T n, allocator_type const& a = allocator_type()) noexcept
		    : node_(a)
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

		constexpr explicit basic_json(string_type v, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, rawptr, std::move(v));
			stor().str_ = rawptr;
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(char_type const* begin, char_type const* end, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, rawptr, begin, end);
			stor().str_ = rawptr;
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(char_type const* str, string_type::size_type count, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, rawptr, str, count);
			stor().str_ = rawptr;
			guard.release();
			kind(kind_t::string);
		}

		constexpr explicit basic_json(char_type const* str, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, rawptr, str);
			stor().str_ = rawptr;
			guard.release();
			kind(kind_t::string);
		}

		template <typename StrLike>
		    requires std::constructible_from<string_type, StrLike>
		    && (std::is_convertible_v<StrLike const&, char_type const*> == false)
		constexpr basic_json(StrLike const& str, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, rawptr, str);
			stor().str_ = rawptr;
			guard.release();
			kind(kind_t::string);
		}

		constexpr basic_json(array_type arr, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			rollbacker_array_all_ rollbacker(arr);
			detail::node_variant_alloc_guard<array_type, node_type> guard(node_);
			auto ator = array_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			array_ator_traits::construct(ator, rawptr, std::move(arr));
			stor().arr_ = rawptr;
			guard.release();
			rollbacker.release();
			kind(kind_t::array);
		}

		constexpr basic_json(object_type obj, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			rollbacker_map_all_ rollbacker(obj);
			detail::node_variant_alloc_guard<object_type, node_type> guard(node_);
			auto ator = object_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			object_ator_traits::construct(ator, rawptr, std::move(obj));
			stor().obj_ = rawptr;
			guard.release();
			rollbacker.release();
			kind(kind_t::object);
		}

		constexpr basic_json(node_type&& n) noexcept
		    : node_(std::move(n))
		{
			n.kind_ = kind_t{};
			n.stor_ = stor_t{};
		}

		constexpr basic_json(node_type&& n, allocator_type const& a) noexcept
		    : node_(a)
		{
			kind(std::exchange(n.kind_, kind_t{}));
			stor() = std::exchange(n.stor_, stor_t{});
		}

		constexpr allocator_type get_allocator() const noexcept
		{
			return node_.get_allocator_ref();
		}

		[[nodiscard("discard nodes will cause leaks")]] constexpr operator node_type() && noexcept
		{
			auto node = node_;
			kind(kind_t{});
			stor() = {};

			return node;
		}

	private:
		static constexpr void clone_node(node_type& lhs, node_type const& rhs)
		{
			auto rk = rhs.kind_;
			auto const& rs = rhs.stor_;
			auto& s = lhs.stor_;

			switch (rk)
			{
			case kind_t::string: {
				auto const& rstr = *static_cast<string_type const*>(rs.str_);
				detail::node_variant_alloc_guard<string_type, node_type> mem(lhs);
				auto ator = string_ator(lhs.get_allocator_ref());
				auto lptr = mem.get();
				string_ator_traits::construct(ator, lptr, rstr);
				mem.release();
				s.str_ = lptr;
				break;
			}
			case kind_t::array: {
				if constexpr (std::is_trivially_copyable_v<allocator_type>)
				{
					auto const& rarr = *static_cast<array_type const*>(rs.arr_);
					detail::node_variant_alloc_guard<array_type, node_type> guard(lhs);
					auto ator = array_ator(lhs.get_allocator_ref());
					auto lptr = guard.get();
					array_ator_traits::construct(ator, lptr, rarr);
					array_type& larr{ *lptr };
					auto sentry = larr.begin();
					rollbacker_array_part_ rollbacker(sentry, larr);

					auto first = rarr.begin();
					auto last = rarr.end();
					for (; first != last; ++sentry, ++first)
					{
						clone_node(*sentry, *first);
					}

					guard.release();
					s.arr_ = lptr;
				}
				else
				{
					auto const& rarr = *static_cast<array_type const*>(rs.arr_);
					detail::node_variant_alloc_guard<array_type, node_type> guard(lhs);
					auto ator = array_ator(lhs.get_allocator_ref());
					auto lptr = guard.get();
					array_ator_traits::construct(ator, lptr);
					array_type& larr{ *lptr };
					larr.reserve(rarr.size());
					rollbacker_array_all_ rollbacker(larr);

					auto first = rarr.begin();
					auto last = rarr.end();
					for (; first != last; ++first)
					{
						basic_json temp{ lhs.node_.get_allocator() };
						clone_node(temp.node_, *first);
						larr.push_back(std::move(temp));
					}

					guard.release();
					s.arr_ = lptr;
				}
				break;
			}
			case kind_t::object: {
				auto const& robj = *static_cast<object_type const*>(rs.obj_);
				detail::node_variant_alloc_guard<object_type, node_type> guard(lhs);
				auto ator = object_ator(lhs.get_allocator_ref());
				auto lptr = guard.get();
				object_ator_traits::construct(ator, lptr);
				object_type& lobj{ *lptr };
				rollbacker_map_all_ rollbacker(lobj);

				auto first = robj.begin();
				auto last = robj.end();

				for (; first != last; ++first)
				{
					auto const& [rkey, rvalue] = *first;
					basic_json temp{ lhs.get_allocator_ref() };
					clone_node(temp.node_, rvalue);
					lobj.emplace(rkey, std::move(temp.node_));
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

			lhs.kind_ = rk;
		}

		constexpr void clone(const basic_json& rhs)
		{
			clone_node(node_, rhs.node_);
		}

	public:
		constexpr ~basic_json() noexcept
		{
			clear();
		}

		constexpr slice_type slice() noexcept
		{
			return *this;
		}

		constexpr const_slice_type slice() const noexcept
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
			pair arr[N];

			constexpr operator basic_json() &&
			{
				object_type obj;
				for (auto& i : arr)
				{
					obj.emplace(std::move(i.key), std::move(i.value));
				}

				return obj;
			}
		};

		// deduction guide for object{...}
		template <typename... Ts>
		object(Ts&&...) -> object<get_pair_num(type_list<Ts...>{})>;

		template <std::size_t N>
		struct array
		{
			basic_json arr[N];

			constexpr operator basic_json() &&
			{
				array_type arr_;
				for (auto& value : arr)
				{
					arr_.reserve(N);
					arr_.push_back(std::move(value));
				}

				return arr_;
			}
		};

		// deduction guide for array{...}
		template <typename... Ts>
		array(Ts&&...) -> array<sizeof...(Ts)>;
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

	using json = basic_json<>;
	using json_slice = basic_json_slice<>;
	using const_json_slice = basic_const_json_slice<>;

	namespace pmr
	{
		template <typename Number = double, typename Integer = long long, typename UInteger = unsigned long long>
		using basic_json_node = bizwen::basic_json_node<Number, Integer, UInteger, std::pmr::polymorphic_allocator<char>>;

		template <typename Node = pmr::basic_json_node<>, typename String = std::pmr::string,
		    typename Array = std::pmr::vector<Node>,
		    typename Object = std::pmr::map<String, Node>,
		    bool HasInteger = true, bool HasUInteger = true>
		using basic_json_slice = bizwen::basic_json_slice<Node, String, Array, Object, HasInteger, HasUInteger>;

		template <typename Node = pmr::basic_json_node<>, typename String = std::pmr::string,
		    typename Array = std::pmr::vector<Node>,
		    typename Object = std::pmr::map<String, Node>,
		    bool HasInteger = true, bool HasUInteger = true>
		using basic_const_json_slice = bizwen::basic_const_json_slice<Node, String, Array, Object, HasInteger, HasUInteger>;

		template <typename Node = pmr::basic_json_node<>, typename String = std::pmr::string,
		    typename Array = std::pmr::vector<Node>,
		    typename Object = std::pmr::map<String, Node>,
		    bool HasInteger = true, bool HasUInteger = true>
		using basic_json = bizwen::basic_json<Node, String, Array, Object, HasInteger, HasUInteger>;

		using json = basic_json<>;
		using json_slice = basic_json_slice<>;
		using const_json_slice = basic_const_json_slice<>;
	} // namespace pmr
} // namespace bizwen

// these class templates have nested allocator_type, but shoudn't be uses-allocator constructed

template <typename Number, typename Integer, typename UInteger, typename Allocator, typename Alloc2>
struct std::uses_allocator<bizwen::basic_json_node<Number, Integer, UInteger, Allocator>, Alloc2>: std::false_type
{
};

template <typename Node, typename String, typename Array, typename Object, bool HasInteger, bool HasUInteger, typename Alloc>
struct std::uses_allocator<bizwen::basic_json_slice<Node, String, Array, Object, HasInteger, HasUInteger>, Alloc>: std::false_type
{
};

template <typename Node, typename String, typename Array, typename Object, bool HasInteger, bool HasUInteger, typename Alloc>
struct std::uses_allocator<bizwen::basic_const_json_slice<Node, String, Array, Object, HasInteger, HasUInteger>, Alloc>: std::false_type
{
};

#endif // defined(BIZWEN_BASIC_JSON_HPP)
