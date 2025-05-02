#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>
#include <variant>
#include <string>
#include <vector>
#include <map>

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

struct nulljson_t
{
	explicit constexpr nulljson_t() noexcept = default;
};

struct simple_json_node
{
	using variant_type = std::variant<std::monostate, nulljson_t, bool, double, long long, unsigned long long,
	    std::string*, std::vector<simple_json_node>*, std::map<std::string, simple_json_node>*>;
	using allocator_type = std::allocator<std::byte>;

	variant_type stor;
	allocator_type alloc;
};

struct node_traits
{
	using node_type = simple_json_node;
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
	template <typename T> using rebind_traits = std::allocator_traits<allocator_type>::rebind_traits<T>;
	static_assert(!is_string_view && std::is_same_v<rebind_traits<string_type>::pointer, raw_string_type>);
	// unused for support winrt::hstring and Qstring
	// static_assert(is_string_view && std::is_trivially_copy_constructible_v<string_type>
	//    && std::is_trivially_copy_assignable_v<string_type> && std::is_trivially_destructible_v<string_type>);
	static_assert(std::is_same_v<rebind_traits<array_type>::pointer, raw_array_type>);
	static_assert(std::is_same_v<rebind_traits<object_type>::pointer, raw_object_type>);
	using key_string_type = object_type::key_type;
	using key_char_type = key_string_type::value_type;
	static_assert(std::integral<key_char_type>);
	using map_node_type = object_type::node_type;

	template <typename T> static void rebind_deallocate(allocator_type const& a, T ptr) noexcept
	{
		auto ra = std::allocator_traits<allocator_type>::rebind_alloc<T>(a);
		// std::allocator_traits<decltype(ra)>::deallocate(ra, ptr, 1uz);
	}
	template <typename T, typename... Ts> static auto rebind_allocate_construct(allocator_type const& a, Ts... ts)
	{
		auto ra = std::allocator_traits<allocator_type>::rebind_alloc<T>(a);
		auto addr = rebind_traits<T>::allocate(ra, 1uz);
		rebind_traits<T>::construct(ra, addr, std::forward<Ts>(ts)...);
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

	static auto& get_boolean(variant_type& stor) { return *std::get_if<bool>(&stor); }
	static auto& get_number(variant_type& stor) { return *std::get_if<number_type>(&stor); }
	static auto& get_integer(variant_type& stor) { return *std::get_if<integer_type>(&stor); }
	static auto& get_uinteger(variant_type& stor) { return *std::get_if<uinteger_type>(&stor); }
	static auto& get_raw_string(variant_type& stor) { return *std::get_if<raw_string_type>(&stor); }
	static auto& get_raw_array(variant_type& stor) { return *std::get_if<raw_array_type>(&stor); }
	static auto& get_raw_object(variant_type& stor) { return *std::get_if<raw_object_type>(&stor); }

	static bool try_reset_union(variant_type& stor, allocator_type& alloc, kind_t new_kind) noexcept
	{
		auto kind = get_kind(stor);
		if (kind == new_kind)
			return false;

		using enum kind_t;
		switch (kind)
		{
		case kind_t::string:
			if constexpr (!is_string_view)
				rebind_deallocate(alloc, get_raw_string(stor));
		case kind_t::array:
			rebind_deallocate(alloc, get_raw_array(stor));
		case kind_t::object:
			rebind_deallocate(alloc, get_raw_object(stor));
		default:;
		}
		return true;
	}

	static void set_undefined(variant_type& stor, allocator_type& alloc)
	{
		if (try_reset_union(stor, alloc, kind_t::undefined))
			stor.emplace<std::monostate>();
	}

	static void set_null(variant_type& stor, allocator_type& alloc)
	{
		if (try_reset_union(stor, alloc, kind_t::null))
			stor.emplace<nulljson_t>();
	}

	static void set_boolean(variant_type& stor, allocator_type& alloc, bool value) noexcept
	{
		if (try_reset_union(stor, alloc, kind_t::boolean))
			stor.emplace<bool>(value);
		else
			get_boolean(stor) = value;
	}

	static void set_integer(variant_type& stor, allocator_type& alloc, integer_type value) noexcept
	{
		if (try_reset_union(stor, alloc, kind_t::integer))
			stor.emplace<integer_type>(value);
		else
			get_integer(stor) = value;
	}

	static void set_uinteger(variant_type& stor, allocator_type& alloc, uinteger_type value) noexcept
	{
		if (try_reset_union(stor, alloc, kind_t::uinteger))
			stor.emplace<uinteger_type>(value);
		else
			get_uinteger(stor) = value;
	}

	static void set_string(variant_type& stor, allocator_type& alloc, string_type value) noexcept
	{
		if (try_reset_union(stor, alloc, kind_t::string))
		{
			if constexpr (is_string_view)
				;
			// stor.emplace<string_type>(value);
			else
				rebind_allocate_construct<string_type>(alloc, std::forward<string_type>(value));
		}
		else
		{
			if constexpr (is_string_view)
				;
			// get_raw_string(stor) = std::forward<string_type>(value);
			else
				*get_raw_string(stor) = std::forward<string_type>(value);
		}
	}

	static void set_array(variant_type& stor, allocator_type& alloc, array_type value) noexcept
	{
		if (try_reset_union(stor, alloc, kind_t::string))
			rebind_allocate_construct<array_type>(alloc, std::forward<array_type>(value));
		else
			*get_raw_array(stor) = std::forward<array_type>(value);
	}

	static void set_object(variant_type& stor, allocator_type& alloc, object_type value) noexcept
	{
		if (try_reset_union(stor, alloc, kind_t::string))
			rebind_allocate_construct<object_type>(alloc, std::forward<object_type>(value));
		else
			*get_raw_object(stor) = std::forward<object_type>(value);
	}
};
