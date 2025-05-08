#ifndef BIZWEN_BASIC_JSON_HPP
#define BIZWEN_BASIC_JSON_HPP

#include <cassert>
#include <cstddef>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <type_traits>
#include <utility>
#include <concepts>
#include <ranges>
#include <variant>

// https://www.rfc-editor.org/rfc/rfc8259

namespace bizwen
{
	struct nulljson_t
	{
		explicit constexpr nulljson_t() noexcept = default;
	};

	inline constexpr nulljson_t nulljson{};

	// https://cplusplus.github.io/LWG/issue3917
	// since the types of string, array and map are unknown at this point,
	// memory allocation can only be done by instantiating with std::byte.

	template <typename Node, bool HasInteger = true, bool HasUInteger = true> class basic_json;

	template <typename Node, bool HasInteger = true, bool HasUInteger = true> class basic_const_json_slice;

	template <typename Node, bool HasInteger = true, bool HasUInteger = true> class basic_json_slice;

	enum class json_errc
	{
		// for accessor
		is_undefined = 1,
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
		is_empty,
		not_undefined_or_null,
		not_undefined_or_boolean,
		not_undefined_or_number,
		not_undefined_or_integer,
		not_undefined_or_uinteger,
		not_undefined_or_string,
		not_undefined_or_array,
		not_undefined_or_object,
		// for deserializer
		syntax_error,
		number_overflow,
		// for serializer
		number_nan,
		number_inf
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

		char const* what() const noexcept override
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
			case json_errc::not_undefined_or_null:
				return "JSON error: current value is not undefined or not null.";
			case json_errc::not_undefined_or_boolean:
				return "JSON error: current value is not undefined or not bool.";
			case json_errc::not_undefined_or_number:
				return "JSON error: current value is not undefined or not a number.";
			case json_errc::not_undefined_or_string:
				return "JSON error: current value is not undefined or not a string.";
			default:
				return "JSON error: unspecified error.";
			}
		}

	private:
		json_errc code_;
	};

	namespace detail
	{
		enum class kind_t : unsigned char
		{
			undefined,
			null,
			boolean,
			number,
			integer,
			uinteger,
			string,
			array,
			object
		};

		template <typename Node> struct node_traits
		{
			using node_type = Node;
			using variant_type = node_type::variant_type;
			using allocator_type = node_type::allocator_type;
			static_assert(std::variant_size_v<variant_type> == 9uz);
			static_assert(std::is_same_v<std::monostate, std::variant_alternative_t<0uz, variant_type>>);
			static_assert(std::is_same_v<nulljson_t, std::variant_alternative_t<1uz, variant_type>>);
			static_assert(std::is_same_v<bool, std::variant_alternative_t<2uz, variant_type>>);
			using number_type = std::variant_alternative_t<3uz, variant_type>;
			static_assert(std::floating_point<number_type>);
			using integer_type = std::variant_alternative_t<4uz, variant_type>;
			static_assert(std::signed_integral<integer_type>);
			using uinteger_type = std::variant_alternative_t<5uz, variant_type>;
			static_assert(std::unsigned_integral<uinteger_type>);
			using raw_string_type = std::variant_alternative_t<6uz, variant_type>;

			template <typename T>
			using defancy_remove_pointer_t = std::remove_pointer_t<decltype(std::to_address(std::declval<T>()))>;

			template <typename S> struct get_string_type
			{
				using type = S;
			};

			template <typename S>
			    requires(!requires { typename S::value_type; })
			struct get_string_type<S>
			{
				using type = defancy_remove_pointer_t<S>;
			};

			using string_type = get_string_type<raw_string_type>::type;
			using char_type = string_type::value_type;
			static_assert(std::integral<char_type>);
			static inline constexpr bool is_string_view = std::is_same_v<string_type, raw_string_type>;
			using raw_array_type = std::variant_alternative_t<7uz, variant_type>;
			using array_type = defancy_remove_pointer_t<raw_array_type>;
			using raw_object_type = std::variant_alternative_t<8uz, variant_type>;
			using object_type = defancy_remove_pointer_t<raw_object_type>;

			template <typename T>
			using rebind_traits = std::allocator_traits<allocator_type>::template rebind_traits<T>;

			static_assert(
			    !is_string_view && std::is_same_v<typename rebind_traits<string_type>::pointer, raw_string_type>);
			// unused for support winrt::hstring and Qstring
			// static_assert(is_string_view && std::is_trivially_copy_constructible_v<string_type>
			//    && std::is_trivially_copy_assignable_v<string_type> && std::is_trivially_destructible_v<string_type>);
			static_assert(std::is_same_v<typename rebind_traits<array_type>::pointer, raw_array_type>);
			static_assert(std::is_same_v<typename rebind_traits<object_type>::pointer, raw_object_type>);
			using key_string_type = object_type::key_type;
			using key_char_type = key_string_type::value_type;
			static_assert(std::integral<key_char_type>);
			using map_node_type = object_type::node_type;

			template <typename T> static void rebind_deallocate(allocator_type const& a, T ptr) noexcept
			{
				using U = std::remove_reference_t<decltype(*ptr)>;
				ptr->~U();
				using A = std::allocator_traits<allocator_type>::template rebind_alloc<U>;
				auto ra = A(a);
				std::allocator_traits<decltype(ra)>::deallocate(ra, ptr, 1uz);
			}

			template <typename T, typename... Ts>
			static auto rebind_allocate_construct(allocator_type const& a, Ts... ts)
			{
				using A = std::allocator_traits<allocator_type>::template rebind_alloc<T>;
				auto ra = A(a);
				auto addr = rebind_traits<T>::allocate(ra, 1uz);
				struct guard
				{
					decltype(addr)& ptr;
					A& a;
					bool released;

					~guard()
					{
						if (!released)
							std::allocator_traits<A>::deallocate(a, ptr, 1uz);
					}
				};

				guard g{ addr, ra, false };
				rebind_traits<T>::construct(ra, addr, std::forward<Ts>(ts)...);
				g.released = true;

				return addr;
			}

			static kind_t get_kind(variant_type const& stor) noexcept
			{
				if (std::holds_alternative<std::monostate>(stor))
					return kind_t::undefined;
				if (std::holds_alternative<nulljson_t>(stor))
					return kind_t::null;
				if (std::holds_alternative<bool>(stor))
					return kind_t::boolean;
				if (std::holds_alternative<number_type>(stor))
					return kind_t::number;
				if (std::holds_alternative<integer_type>(stor))
					return kind_t::integer;
				if (std::holds_alternative<uinteger_type>(stor))
					return kind_t::uinteger;
				if (std::holds_alternative<raw_string_type>(stor))
					return kind_t::string;
				if (std::holds_alternative<raw_array_type>(stor))
					return kind_t::undefined;
				if (std::holds_alternative<raw_object_type>(stor))
					return kind_t::object;
				std::unreachable();
			}

			template <typename T> static T& get_raw(variant_type& stor) noexcept { return *std::get_if<T>(&stor); }

			template <typename T> static const T& get_raw(variant_type const& stor) noexcept
			{
				return *std::get_if<T>(&stor);
			}

			template <typename T, typename V> static auto& get_val(V& stor) noexcept
			{
				if constexpr (std::is_same_v<string_type, T> && is_string_view)
					return get_raw<raw_string_type>(stor);
				else if constexpr (std::is_same_v<string_type, T> && !is_string_view)
					return *get_raw<raw_string_type>(stor);
				else if constexpr (std::is_same_v<array_type, T>)
					return *get_raw<raw_array_type>(stor);
				else if constexpr (std::is_same_v<object_type, T>)
					return *get_raw<raw_object_type>(stor);
				else
					return get_raw<T>(stor);
			}

			static void clear_variant(variant_type& stor, allocator_type& alloc, kind_t old_kind) noexcept
			{
				using enum kind_t;

				switch (old_kind)
				{
				case kind_t::string:
					if constexpr (!is_string_view)
					{
						auto ptr = get_raw<raw_string_type>(stor);
						rebind_deallocate(alloc, ptr);
					}
				case kind_t::array: {
					auto ptr = get_raw<raw_array_type>(stor);
					for (auto& i : *ptr)
					{
						clear_variant(i.stor, i.alloc, get_kind(i.stor));
					}
					rebind_deallocate(alloc, ptr);
				}
				case kind_t::object: {
					auto ptr = get_raw<raw_object_type>(stor);
					for (auto& [key, value] : *ptr)
					{
						clear_variant(value.stor, value.alloc, get_kind(value.stor));
					}
					rebind_deallocate(alloc, ptr);
				}
				default:;
				}
			}

			static void set_undefined(variant_type& stor) { stor.template emplace<std::monostate>(); }

			static void set_null(variant_type& stor) { stor.template emplace<nulljson_t>(); }

			static void set_boolean(variant_type& stor, bool value) noexcept { stor.template emplace<bool>(value); }

			static void set_number(variant_type& stor, number_type value) noexcept
			{
				stor.template emplace<number_type>(value);
			}

			static void set_integer(variant_type& stor, integer_type value) noexcept
			{
				stor.template emplace<integer_type>(value);
			}

			static void set_uinteger(variant_type& stor, uinteger_type value) noexcept
			{
				stor.template emplace<uinteger_type>(value);
			}

			template <typename... Ts>
			static void set_string(variant_type& stor, allocator_type& alloc, Ts&&... ts) noexcept
			{
				if constexpr (is_string_view)
					stor.template emplace<string_type>(std::forward<Ts...>(ts)...);
				else
					stor.template emplace<raw_string_type>(rebind_allocate_construct<string_type>(
					    alloc, std::forward<string_type>(std::forward<Ts...>(ts)...)));
			}

			template <typename... Ts>
			static void set_array(variant_type& stor, allocator_type& alloc, Ts&&... ts) noexcept
			{
				stor.template emplace<raw_array_type>(
				    rebind_allocate_construct<array_type>(alloc, std::forward<Ts...>(ts)...));
			}

			template <typename... Ts>
			static void set_object(variant_type& stor, allocator_type& alloc, Ts&&... ts) noexcept
			{
				stor.template emplace<raw_object_type>(
				    rebind_allocate_construct<object_type>(alloc, std::forward<Ts...>(ts)...));
			}

			static void copy_variant(variant_type const& from, variant_type& to, allocator_type& alloc)
			{
				switch (get_kind(from))
				{
				case kind_t::undefined:
					break;
				case kind_t::null:
					to.template emplace<nulljson_t>();
					break;
				case kind_t::boolean:
					to.template emplace<bool>();
					break;
				case kind_t::number:
					to.template emplace<number_type>(get_val<number_type>(from));
					break;
				case kind_t::integer:
					to.template emplace<integer_type>(get_val<integer_type>(from));
					break;
				case kind_t::uinteger:
					to.template emplace<uinteger_type>(get_val<uinteger_type>(from));
					break;
				case kind_t::string:
					if constexpr (is_string_view)
						to.template emplace<string_type>(get_val<raw_string_type>(from));
					else
						to.template emplace<raw_string_type>(
						    rebind_allocate_construct<string_type>(alloc, *get_raw<raw_string_type>(from)));
					break;
				case kind_t::array:
					to.template emplace<raw_array_type>(rebind_allocate_construct<array_type>(alloc, std::from_range,
					    (*get_raw<raw_array_type>(from)) | std::views::transform([](node_type const& lhs) static {
						    node_type rhs{ std::monostate{}, lhs.alloc };
						    copy_variant(lhs.stor, rhs.stor, rhs.alloc);

						    return rhs;
					    })));
					break;
				case kind_t::object:
					to.template emplace<raw_object_type>(rebind_allocate_construct<object_type>(alloc, std::from_range,
					    (*get_raw<raw_object_type>(from))
					        | std::views::transform([](object_type::value_type const& lhs) {
						          auto&& [key, value] = lhs;
						          typename object_type::value_type rhs{ key, { std::monostate{}, value.alloc } };
						          auto&& [_, out_value] = rhs;
						          copy_variant(value.stor, out_value.stor, out_value.alloc);

						          return rhs;
					          })));
					break;
				}
			}
		};

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
		concept transparently_comparable_json_object
		    = transparently_comparable_associative<Object> || transparently_comparable_unordered_associative<Object>;

		template <typename KeyStrLike, typename Object>
		concept noncovertible_to_key_char_cptr
		    = !std::is_convertible_v<KeyStrLike const&, typename Object::key_type::value_type const*>;

		template <typename Node, bool HasInteger, bool HasUInteger> struct basic_json_slice_common_base
		{
		private:
			using node_traits_t = detail::node_traits<Node>;

		public:
			static inline constexpr bool has_integer = HasInteger;
			static inline constexpr bool has_uinteger = HasUInteger;

			using node_type = Node;
			using value_type = Node;
			using object_type = node_traits_t::object_type;
			using array_type = node_traits_t::array_type;
			using string_type = node_traits_t::string_type;
			using number_type = node_traits_t::number_type;
			using integer_type = node_traits_t::integer_type;
			using uinteger_type = node_traits_t::uinteger_type;

			using const_slice_type = basic_const_json_slice<node_type, has_integer, has_uinteger>;
			using slice_type = basic_json_slice<node_type, has_integer, has_uinteger>;
			using json_type = basic_json<node_type, has_integer, has_uinteger>;
			friend json_type;

			using char_type = node_traits_t::char_type;
			using map_node_type = node_traits_t::map_node_type;
			using allocator_type = node_traits_t::allocator_type;
			using key_string_type = node_traits_t::key_string_type;
			using key_char_type = node_traits_t::key_char_type;

			node_type* node_{}; // made private in derived classes

			constexpr kind_t kind() const { return node_traits_t::get_kind(node_->stor); }

			template <typename T> constexpr auto& get_val() { return node_traits_t::template get_val<T>(node_->stor); }

			template <typename T> constexpr auto& get_val() const
			{
				return node_traits_t::template get_val<T>(node_->stor);
			}

		public:
			[[nodiscard]] constexpr bool empty() const noexcept { return node_ != nullptr; }

			[[nodiscard]] constexpr bool undefined() const noexcept { return kind() == kind_t::undefined; }

			[[nodiscard]] constexpr bool string() const noexcept { return kind() == kind_t::string; }

			[[nodiscard]] constexpr bool null() const noexcept { return kind() == kind_t::null; }

			[[nodiscard]] constexpr bool boolean() const noexcept { return kind() == kind_t::boolean; }

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

			[[nodiscard]] constexpr bool object() const noexcept { return kind() == kind_t::object; }

			[[nodiscard]] constexpr bool array() const noexcept { return kind() == kind_t::array; }

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
				auto k = kind();

				if (!k == kind_t::boolean)
					throw json_error(json_errc::not_boolean);

				return get_val<bool>();
			}

			constexpr explicit operator number_type() const
			{
				auto k = kind();

				if (k == kind_t::number)
					return get_val<number_type>();
				else if (k == kind_t::integer)
					return get_val<integer_type>();
				else if (k == kind_t::uinteger)
					return get_val<uinteger_type>();

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

				return get_val<string_type>();
			}

			constexpr explicit operator array_type const&() const&
			{
				if (!array())
					throw json_error(json_errc::not_array);

				return get_val<array_type>();
			}

			constexpr explicit operator object_type const&() const&
			{
				if (!object())
					throw json_error(json_errc::not_object);

				return get_val<object_type>();
			}

			constexpr explicit operator integer_type() const
			    requires HasInteger
			{
				if (!integer())
					throw json_error(json_errc::not_integer);

				return get_val<integer_type>();
			}

			constexpr explicit operator uinteger_type() const
			    requires HasUInteger
			{
				if (!uinteger())
					throw json_error(json_errc::not_uinteger);

				return get_val<uinteger_type>();
			}

			// defined after the class body of basic_const_json_slice

			constexpr auto operator[](key_string_type const& k) const -> const_slice_type;

			template <typename KeyStrLike>
			    requires transparently_comparable_json_object<object_type>
			    && noncovertible_to_key_char_cptr<KeyStrLike, object_type>
			constexpr auto operator[](KeyStrLike const& k) const -> const_slice_type;

			constexpr auto operator[](key_char_type const* k) const -> const_slice_type;

			template <std::integral T> constexpr auto operator[](T pos) const -> const_slice_type;

			constexpr auto as_array() const
			{
				constexpr auto node_to_slice
				    = [](node_type const& node) static noexcept { return const_slice_type{ node }; };

				return static_cast<array_type const&>(*this) | std::views::transform(node_to_slice);
			}

			constexpr auto as_object() const
			{
				constexpr auto pair_node_to_slice = [](object_type::value_type const& pair) static noexcept {
					auto& [key, value]{ pair };
					return std::pair<key_string_type const&, const_slice_type>{ key, value };
				};

				return static_cast<object_type const&>(*this) | std::views::transform(pair_node_to_slice);
			}
		};
	}

	template <typename Node, bool HasInteger, bool HasUInteger>
	class basic_const_json_slice: public detail::basic_json_slice_common_base<Node, HasInteger, HasUInteger>
	{
	private:
		using base_type = detail::basic_json_slice_common_base<Node, HasInteger, HasUInteger>;
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

		friend constexpr void swap(basic_const_json_slice& lhs, basic_const_json_slice& rhs) noexcept { lhs.swap(rhs); }

		// Similar to iterators, default construction is allowed, but except for operator=,
		// operations on default-constructed slice cause undefined behavior.
		// The default constructor is intentionally provided for default arguments.
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
		template <typename Node, bool HasInteger, bool HasUInteger>
		constexpr auto basic_json_slice_common_base<Node, HasInteger, HasUInteger>::operator[](
		    key_string_type const& k) const -> const_slice_type
		{
			if (!object())
				throw json_error(json_errc::nonobject_indexing);

			auto const& o = get_val<object_type>();
			auto i = o.find(k);

			if (i == o.end())
				throw json_error(json_errc::key_not_found);

			auto& [_, v] = *i;

			return v;
		}

		template <typename Node, bool HasInteger, bool HasUInteger>
		template <typename KeyStrLike>
		    requires transparently_comparable_json_object<typename node_traits<Node>::object_type>
		    && noncovertible_to_key_char_cptr<KeyStrLike, typename node_traits<Node>::object_type>
		constexpr auto basic_json_slice_common_base<Node, HasInteger, HasUInteger>::operator[](
		    KeyStrLike const& k) const -> const_slice_type
		{
			if (!object())
				throw json_error(json_errc::nonobject_indexing);

			auto const& o = get_val<object_type>();
			auto i = o.find(k);

			if (i == o.end())
				throw json_error(json_errc::key_not_found);

			auto& [_, v] = *i;

			return v;
		}

		template <typename Node, bool HasInteger, bool HasUInteger>
		constexpr auto basic_json_slice_common_base<Node, HasInteger, HasUInteger>::operator[](
		    key_char_type const* k) const -> const_slice_type
		{
			if (!object())
				throw json_error(json_errc::nonobject_indexing);

			auto const& o = get_val<object_type>();
			auto i = o.find(k);

			if (i == o.end())
				throw json_error(json_errc::key_not_found);

			auto& [_, v] = *i;

			return v;
		}

		template <typename Node, bool HasInteger, bool HasUInteger>
		template <std::integral T>
		constexpr auto basic_json_slice_common_base<Node, HasInteger, HasUInteger>::operator[](T pos) const
		    -> const_slice_type
		{
			if (!array())
				throw json_error(json_errc::nonarray_indexing);

			auto const& a = get_val<array_type>();

			return a[pos];
		}
	}

	template <typename Node, bool HasInteger, bool HasUInteger>
	class basic_json_slice: public detail::basic_json_slice_common_base<Node, HasInteger, HasUInteger>
	{
	private:
		using base_type = detail::basic_json_slice_common_base<Node, HasInteger, HasUInteger>;
		using base_type::node_;
		using node_traits_t = detail::node_traits<Node>;
		using base_type::kind;

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
		using base_type::integer;
		using base_type::null;
		using base_type::number;
		using base_type::object;
		using base_type::string;
		using base_type::uinteger;
		using base_type::undefined;

		constexpr void swap(basic_json_slice& rhs) noexcept
		{
			auto temp = node_;
			node_ = rhs.node_;
			rhs.node_ = temp;
		}

		friend constexpr void swap(basic_json_slice& lhs, basic_json_slice& rhs) noexcept { lhs.swap(rhs); }

		// Similar to iterators, default construction is allowed, but except for operator=,
		// operations on default-constructed slice cause undefined behavior.
		// The default constructor is intentionally provided for default arguments.
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

			return base_type::template get_val<string_type>();
		}

		constexpr explicit operator array_type&() &
		{
			if (!array())
				throw json_error(json_errc::not_array);

			return base_type::template get_val<array_type>();
		}

		constexpr explicit operator object_type&() &
		{
			if (!object())
				throw json_error(json_errc::not_object);

			return base_type::template get_val<object_type>();
		}

	public:
		using base_type::operator[];

		constexpr void reset() noexcept
		{
			assert(node_);
			node_traits_t::clear_variant(node_->stor, node_->alloc, kind());
			node_traits_t::set_undefined(node_->stor);
		}

		constexpr basic_json_slice operator[](key_string_type const& k)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == object || kd == undefined))
				throw json_error(json_errc::nonobject_indexing);

			node_traits_t::set_object(node_->stor, node_->alloc);

			auto& o = base_type::template get_val<object_type>();
			auto [i, _] = o.emplace(k, node_type{ std::monostate{}, node_->alloc });
			auto& [unused, v] = *i;

			return v;
		}

		template <typename KeyStrLike>
		    requires detail::transparently_comparable_json_object<object_type>
		    && detail::noncovertible_to_key_char_cptr<KeyStrLike, object_type>
		constexpr basic_json_slice operator[](KeyStrLike const& k)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == object || kd == undefined))
				throw json_error(json_errc::nonobject_indexing);

			node_traits_t::set_object(node_->stor, node_->alloc);

			auto& o = base_type::template get_val<object_type>();
			auto [i, _] = o.emplace(k, node_type{ std::monostate{}, node_->alloc });
			auto& [unused, v] = *i;

			return v;
		}

		constexpr basic_json_slice operator[](key_char_type const* k)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == object || kd == undefined))
				throw json_error(json_errc::nonobject_indexing);

			node_traits_t::set_object(node_->stor, node_->alloc);

			auto& o = base_type::template get_val<object_type>();
			auto [i, _] = o.emplace(k, node_type{ std::monostate{}, node_->alloc });
			auto& [unused, v] = *i;

			return v;
		}

		template <std::integral T> constexpr basic_json_slice operator[](T pos)
		{
			if (!array())
				throw json_error(json_errc::nonarray_indexing);

			auto& a = base_type::template get_val<array_type>();

			return a[pos];
		}

		constexpr basic_json_slice& operator=(string_type const& str)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == string || kd == undefined))
				throw json_error(json_errc::not_undefined_or_string);

			if (kd == string)
			{
				base_type::template get_val<string_type>() = str;
			}
			else // undefined
			{
				node_traits_t::set_string(node_->stor, node_->alloc, str);
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(string_type&& str)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == string || kd == undefined))
				throw json_error(json_errc::not_undefined_or_string);

			if (kd == string)
			{
				base_type::template get_val<string_type>() = std::move(str);
			}
			else // undefined
			{
				node_traits_t::set_string(node_->stor, node_->alloc, str);
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(char_type const* str)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == string || kd == undefined))
				throw json_error(json_errc::not_undefined_or_string);

			if (kd == string)
			{
				base_type::template get_val<string_type>() = str;
			}
			else // undefined
			{
				node_traits_t::set_string(node_->stor, node_->alloc, str);
			}

			return *this;
		}

		template <typename StrLike>
		    requires std::constructible_from<string_type, StrLike>
		    && (!std::is_convertible_v<StrLike const&, char_type const*>)
		constexpr basic_json_slice& operator=(StrLike const& str)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == string || kd == undefined))
				throw json_error(json_errc::not_undefined_or_string);

			if (kd == string)
			{
				base_type::template get_val<string_type>() = str;
			}
			else // undefined
			{
				node_traits_t::set_string(node_->stor, node_->alloc, str);
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(nulljson_t)
		{
			auto kd = kind();
			using enum detail::kind_t;

			if (!(kd == null || kd == undefined))
				throw json_error(json_errc::not_undefined_or_null);

			node_traits_t::set_null(node_->stor);

			return *this;
		}

		template <typename T>
		    requires std::is_arithmetic_v<T>
		constexpr basic_json_slice& operator=(T n)
		{
			if constexpr (std::same_as<T, bool>)
			{
				auto kd = kind();
				using enum detail::kind_t;

				if (!(kd == boolean || kd == undefined))
					throw json_error(json_errc::not_undefined_or_boolean);

				node_traits_t::set_boolean(node_->stor, n);
			}
			else if constexpr (HasInteger && std::signed_integral<T>)
			{
				auto kd = kind();
				using enum detail::kind_t;

				if (!(kd == integer || kd == undefined))
					throw json_error(json_errc::not_undefined_or_number);

				node_traits_t::set_integer(node_->stor, n);
			}
			else if constexpr (HasUInteger && std::unsigned_integral<T>)
			{
				auto kd = kind();
				using enum detail::kind_t;

				if (!(kd == uinteger || kd == undefined))
					throw json_error(json_errc::not_undefined_or_number);

				node_traits_t::set_uinteger(node_->stor, n);
			}
			else // fallback
			{
				auto kd = kind();
				using enum detail::kind_t;

				if (!(kd == number || kd == undefined))
					throw json_error(json_errc::not_undefined_or_number);

				node_traits_t::set_number(node_->stor, n);
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
			constexpr auto node_to_slice = [](node_type& node) static noexcept { return basic_json_slice{ node }; };

			return static_cast<array_type&>(*this) | std::views::transform(node_to_slice);
		}

		using base_type::as_object;

		constexpr auto as_object()
		{
			constexpr auto pair_node_to_slice = [](object_type::value_type& pair) static noexcept {
				auto& [key, value]{ pair };
				return std::pair<key_string_type const&, basic_json_slice>{ key, value };
			};

			return static_cast<object_type&>(*this) | std::views::transform(pair_node_to_slice);
		}
	};

	template <typename Node, bool HasInteger, bool HasUInteger> class basic_json
	{
		using node_traits_t = detail::node_traits<Node>;

	public:
		static inline constexpr bool has_integer = HasInteger;
		static inline constexpr bool has_uinteger = HasUInteger;

		using node_type = Node;
		using value_type = Node;
		using object_type = node_traits_t::object_type;
		using array_type = node_traits_t::array_type;
		using string_type = node_traits_t::string_type;

		using slice_type = basic_json_slice<node_type, HasInteger, HasUInteger>;
		using const_slice_type = basic_const_json_slice<node_type, HasInteger, HasUInteger>;

		using number_type = node_traits_t::number_type;
		using integer_type = node_traits_t::integer_type;
		using uinteger_type = node_traits_t::uinteger_type;

		using char_type = string_type::value_type;
		using map_node_type = object_type::value_type;
		using allocator_type = node_traits_t::allocator_type;
		using key_string_type = object_type::key_type;
		using key_char_type = key_string_type::value_type;

	private:
		static constexpr bool is_ator_stateless_ = std::allocator_traits<allocator_type>::is_always_equal::value;
		static constexpr bool is_pocca_
		    = std::allocator_traits<allocator_type>::propagate_on_container_copy_assignment::value;
		static constexpr bool is_pocma_
		    = std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value;
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

		node_type node_;

		constexpr detail::kind_t kind() const noexcept { return node_traits_t::get_kind(node_.stor); }

		constexpr void reset() noexcept
		{
			node_traits_t::clear_variant(node_.stor, node_.alloc, kind());
			node_traits_t::set_undefined(node_.stor);
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
					node_traits_t::clear_variant(begin->stor, begin->alloc, node_traits_t::get_kind(begin->stor));
				}
			}
		};

		struct rollbacker_array_all_
		{
			array_type& array;
			bool done;

			constexpr rollbacker_array_all_() noexcept = delete;

			constexpr rollbacker_array_all_(rollbacker_array_all_ const&) noexcept = delete;

			constexpr void release() noexcept { done = true; }

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
					node_traits_t::clear_variant(begin->stor, begin->alloc, node_traits_t::get_kind(begin->stor));
				}
			}
		};

		struct rollbacker_map_all_
		{
			object_type& object;
			bool done;

			constexpr void release() noexcept { done = true; }

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
					node_traits_t::clear_variant(value.stor, value.alloc, node_traits_t::get_kind(value.stor));
				}
			}
		};

		constexpr void swap_without_ator(basic_json& rhs) noexcept
		{
			auto temp = std::move(node_.stor);
			node_.stor = std::move(rhs.node_.stor);
			rhs.node_.stor = std::move(temp);
		}

		constexpr void clone(const basic_json& rhs)
		{
			node_traits_t::copy_variant(rhs.node_.stor, node_.stor, node_.alloc);
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
					swap(node_.alloc, rhs.node_.alloc); // ADL-swap
				}
				else
				{
					// UB if unequal
					assert(node_.alloc == rhs.node_.alloc);
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
		    : node_(std::monostate{}, a)
		{
		}

		constexpr basic_json(basic_json&& rhs) noexcept
		    : node_(std::monostate{}, rhs.node_.alloc)
		{
			rhs.swap_without_ator(*this);
		}
		constexpr basic_json(basic_json&& rhs, allocator_type const& a) noexcept(is_ator_stateless_)
		    : node_(std::monostate{}, a)
		{
			if constexpr (is_ator_stateless_)
			{
				rhs.swap_without_ator(*this);
			}
			else
			{
				if (a == rhs.node_.alloc)
				{
					rhs.swap_without_ator(*this);
				}
				else
				{
					clone(rhs);
					rhs.reset();
				}
			}
		}

		constexpr basic_json(basic_json const& rhs)
		    : node_(std::monostate{},
		          std::allocator_traits<allocator_type>::select_on_container_copy_construction(rhs.node_.alloc))
		{
			clone(rhs);
		}
		constexpr explicit basic_json(basic_json const& rhs, allocator_type const& a)
		    : node_(std::monostate{}, a)
		{
			clone(rhs);
		}

		constexpr basic_json& operator=(basic_json const& rhs)
		{
			if (this != std::addressof(rhs))
			{
				reset();
				if constexpr (!is_ator_stateless_ && is_pocca_)
				{
					node_.alloc = rhs.node_.alloc;
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
				// N.B. ADL-swap may be ill-formed or have undesired effect
				auto tmp_ator = std::move(node_.alloc);
				node_.alloc = std::move(rhs.node_.alloc);
				rhs.node_.alloc = std::move(tmp_ator);
			}
			else
			{
				if (node_.alloc == rhs.node_.alloc)
				{
					rhs.swap_without_ator(*this);
				}
				else
				{
					reset();
					clone(rhs);
					rhs.reset();
				}
			}

			return *this;
		}

		constexpr basic_json(decltype(nullptr), allocator_type const& = allocator_type()) noexcept
		    = delete; // prevent implicit constructing string

		constexpr basic_json(nulljson_t, allocator_type const& a = allocator_type()) noexcept
		    : node_(std::monostate{}, a)
		{
		}

		template <typename T>
		    requires std::is_arithmetic_v<T>
		constexpr basic_json(T n, allocator_type const& a = allocator_type()) noexcept
		    : node_(std::monostate{}, a)
		{
			if constexpr (std::same_as<T, bool>)
			{
				node_traits_t::set_boolean(node_.stor, n);
			}
			else if constexpr (HasInteger && std::signed_integral<T>)
			{
				node_traits_t::set_integer(node_.stor, n);
			}
			else if constexpr (HasUInteger && std::unsigned_integral<T>)
			{
				node_traits_t::set_uinteger(node_.stor, n);
			}
			else // fallback
			{
				node_traits_t::set_number(node_.stor, n);
			}
		}

		constexpr explicit basic_json(string_type v, allocator_type const& a = allocator_type())
		    : node_(std::monostate{}, a)
		{
		}

		constexpr basic_json(
		    char_type const* str, string_type::size_type count, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			node_traits_t::set_string(node_.stor, node_.alloc, str, count);
		}

		constexpr explicit basic_json(char_type const* str, allocator_type const& a = allocator_type())
		    : node_(std::monostate{}, a)
		{
			node_traits_t::set_string(node_.stor, node_.alloc, str);
		}

		template <typename StrLike>
		    requires std::constructible_from<string_type, StrLike>
		    && (std::is_convertible_v<StrLike const&, char_type const*> == false)
		constexpr basic_json(StrLike const& str, allocator_type const& a = allocator_type())
		    : node_(std::monostate{}, a)
		{
			node_traits_t::set_string(node_.stor, node_.alloc, str);
		}

		constexpr basic_json(array_type arr, allocator_type const& a = allocator_type())
		    : node_(std::monostate{}, a)
		{
			rollbacker_array_all_ rollbacker(arr);
			node_traits_t::set_array(node_.stor, node_.alloc, std::move(arr));
			rollbacker.release();
		}

		constexpr basic_json(object_type obj, allocator_type const& a = allocator_type())
		    : node_(std::monostate{}, a)
		{
			rollbacker_map_all_ rollbacker(obj);
			node_traits_t::set_object(node_.stor, node_.alloc, std::move(obj));
			rollbacker.release();
		}

		constexpr basic_json(node_type&& n) noexcept
		    : node_(std::move(n))
		{
		}

		constexpr basic_json(node_type&& n, allocator_type const& a) noexcept
		    : node_(n.stor, a)
		{
			// todo: n.alloc != a
		}

		constexpr allocator_type get_allocator() const noexcept { return node_.alloc; }

		[[nodiscard("discard nodes will cause leaks")]] constexpr operator node_type() && noexcept
		{
			auto node = node_;
			node_.stor.template emplace<std::monostate>();
			return node;
		}

	public:
		constexpr ~basic_json() noexcept { reset(); }

		constexpr slice_type slice() noexcept { return *this; }

		constexpr const_slice_type slice() const noexcept { return *this; }

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

		template <typename... Ts> struct type_list
		{
		};

		template <typename T1, typename T2, typename... Ts>
		static consteval std::size_t get_pair_num_pair(type_list<T1, T2, Ts...>) noexcept
		{
			return 1 + get_pair_num(type_list<Ts...>{});
		}

		template <typename T1, typename... Ts> static consteval std::size_t get_pair_num(type_list<T1, Ts...>) noexcept
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

		static consteval std::size_t get_pair_num(type_list<>) noexcept { return 0; }

	public:
		template <std::size_t N> struct object
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
		template <typename... Ts> object(Ts&&...) -> object<get_pair_num(type_list<Ts...>{})>;

		template <std::size_t N> struct array
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
		template <typename... Ts> array(Ts&&...) -> array<sizeof...(Ts)>;
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
		treat_undefined_as_null = 0x200,
		treat_undefined_as_undefined = 0x400,
		treat_undefined_as_literal = 0x800, // debug only
	};
} // namespace bizwen

// these class templates have nested allocator_type, but shoudn't be uses-allocator constructed

template <typename Node, bool HasInteger, bool HasUInteger, typename Alloc>
struct std::uses_allocator<bizwen::basic_json_slice<Node, HasInteger, HasUInteger>, Alloc>: std::false_type
{
};

template <typename Node, bool HasInteger, bool HasUInteger, typename Alloc>
struct std::uses_allocator<bizwen::basic_const_json_slice<Node, HasInteger, HasUInteger>, Alloc>: std::false_type
{
};

#endif // defined(BIZWEN_BASIC_JSON_HPP)
