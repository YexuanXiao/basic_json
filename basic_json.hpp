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

	// https://cplusplus.github.io/LWG/issue3917
	// since the types of string, array and map are unknown at this point,
	// memory allocation can only be done by instantiating char.
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

		template <typename Alloc>
		using ator_void_ptr_t = std::allocator_traits<Alloc>::void_pointer;

		template <typename T, typename Alloc>
		using ator_rebound_ptr_t = std::allocator_traits<Alloc>::template rebind_traits<T>::pointer;

		template <typename T, typename Node>
		using node_ator_rebound_ptr_t = std::allocator_traits<typename Node::allocator_type>::template rebind_traits<T>::pointer;

		template <typename T, typename Node>
		constexpr node_ator_rebound_ptr_t<T, Node> alloc_from_node(Node& node);

		template <typename T, typename Node>
		constexpr void dealloc_from_node(Node& node, node_ator_rebound_ptr_t<T, Node> p) noexcept;

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

		const char* what() const noexcept override
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
		template <typename VoidPtr, typename FPoint, typename SInt, typename UInt>
		class node_storage_variant
		{
		private:
			static_assert(std::floating_point<FPoint>);
			static_assert(std::signed_integral<SInt>);
			static_assert(std::unsigned_integral<UInt>);

			template <typename Number, typename Integer, typename UInteger, typename Allocator>
			friend class bizwen::basic_json_node;

			template <typename Node, typename String,
			    typename Array,
			    typename Object,
			    bool HasInteger, bool HasUInteger>
			friend class bizwen::basic_json;

			template <typename Node, typename String,
			    typename Array,
			    typename Object,
			    bool HasInteger, bool HasUInteger>
			friend struct detail::basic_json_slice_common_base;

			template <typename T, typename Node>
			friend constexpr detail::node_ator_rebound_ptr_t<T, Node> detail::alloc_from_node(Node& node);

			template <typename T, typename Node>
			friend constexpr void detail::dealloc_from_node(Node& node, detail::node_ator_rebound_ptr_t<T, Node> p) noexcept;

			template <typename T, typename Node>
			friend struct detail::node_variant_alloc_guard;

			template <typename Node, typename String,
			    typename Array,
			    typename Object,
			    bool HasInteger, bool HasUInteger>
			friend class bizwen::basic_const_json_slice;

			template <typename Node, typename String,
			    typename Array,
			    typename Object,
			    bool HasInteger, bool HasUInteger>
			friend class bizwen::basic_json_slice;

			enum class kind_t : unsigned char
			{
				undefined,
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

			union
			{
				VoidPtr str_;
				VoidPtr arr_;
				VoidPtr obj_;
				FPoint num_;
				SInt int_;
				UInt uint_;
			};
			kind_t kind_{};

			constexpr void copy_replace_union(node_storage_variant const& other) noexcept
			{
				// pre: kind_ == other.kind_, no non-trivial variant member of the union constructed
				switch (kind_)
				{
				case kind_t::string:
					std::construct_at(std::addressof(str_), other.str_);
					break;
				case kind_t::array:
					std::construct_at(std::addressof(arr_), other.arr_);
					break;
				case kind_t::object:
					std::construct_at(std::addressof(obj_), other.obj_);
					break;
				case kind_t::number:
					std::construct_at(&num_, other.num_);
					break;
				case kind_t::integer:
					std::construct_at(&int_, other.int_);
					break;
				case kind_t::uinteger:
					std::construct_at(&uint_, other.uint_);
					break;
				}
			}

			constexpr void move_replace_union(node_storage_variant const& other) noexcept
			{
				// pre: kind_ == other.kind_, no non-trivial variant member of the union constructed
				switch (kind_)
				{
				case kind_t::string:
					std::construct_at(std::addressof(str_), std::move(other.str_));
					break;
				case kind_t::array:
					std::construct_at(std::addressof(arr_), std::move(other.arr_));
					break;
				case kind_t::object:
					std::construct_at(std::addressof(obj_), std::move(other.obj_));
					break;
				case kind_t::number:
					std::construct_at(&num_, other.num_);
					break;
				case kind_t::integer:
					std::construct_at(&int_, other.int_);
					break;
				case kind_t::uinteger:
					std::construct_at(&uint_, other.uint_);
					break;
				}
			}

			constexpr void destroy_union() noexcept
			{
				// pre: kind_ indicates the status of the union
				switch (kind_)
				{
				case kind_t::string:
					str_.~VoidPtr();
					break;
				case kind_t::array:
					arr_.~VoidPtr();
					break;
				case kind_t::object:
					obj_.~VoidPtr();
					break;
				}
			}

			constexpr void set_undefined() noexcept
			{
				destroy_union();
				kind_ = kind_t::undefined;
			}

			constexpr void set_null() noexcept
			{
				destroy_union();
				kind_ = kind_t::null;
			}

			constexpr void set_bool(bool b) noexcept
			{
				destroy_union();
				kind_ = b ? kind_t::true_value : kind_t::false_value;
			}

			constexpr void set_num(FPoint x) noexcept
			{
				if (kind_ == kind_t::number)
					num_ = x;
				else
				{
					destroy_union();
					kind_ = kind_t::number;
					std::construct_at(&num_, x);
				}
			}

			constexpr void set_int(SInt n) noexcept
			{
				if (kind_ == kind_t::integer)
					int_ = n;
				else
				{
					destroy_union();
					kind_ = kind_t::integer;
					std::construct_at(&int_, n);
				}
			}

			constexpr void set_uint(UInt n) noexcept
			{
				if (kind_ == kind_t::uinteger)
					num_ = n;
				else
				{
					destroy_union();
					kind_ = kind_t::uinteger;
					std::construct_at(&uint_, n);
				}
			}

			constexpr void set_str_ptr(VoidPtr vp) noexcept
			{
				if (kind_ == kind_t::string)
					str_ = vp;
				else
				{
					destroy_union();
					kind_ = kind_t::string;
					std::construct_at(std::addressof(str_), std::move(vp));
				}
			}

			constexpr void set_arr_ptr(VoidPtr vp) noexcept
			{
				if (kind_ == kind_t::array)
					arr_ = vp;
				else
				{
					destroy_union();
					kind_ = kind_t::array;
					std::construct_at(std::addressof(arr_), std::move(vp));
				}
			}

			constexpr void set_obj_ptr(VoidPtr vp) noexcept
			{
				if (kind_ == kind_t::object)
					obj_ = vp;
				else
				{
					destroy_union();
					kind_ = kind_t::object;
					std::construct_at(std::addressof(obj_), std::move(vp));
				}
			}

		public:
			constexpr node_storage_variant() noexcept {}

			// clang-format off
			node_storage_variant(node_storage_variant const&) noexcept
			    requires std::is_trivially_copy_constructible_v<VoidPtr> = default;
			// clang-format on

			constexpr node_storage_variant(node_storage_variant const& other) noexcept
			    : kind_{ other.kind_ }
			{
				copy_replace_union(other);
			}

			// clang-format off
			node_storage_variant(node_storage_variant&&) noexcept
			    requires std::is_trivially_move_constructible_v<VoidPtr> = default;
			// clang-format on

			constexpr node_storage_variant(node_storage_variant&& other) noexcept
			    : kind_{ other.kind_ }
			{
				move_replace_union(std::move(other));
			}

			// clang-format off
			node_storage_variant& operator=(node_storage_variant const&) noexcept
			    requires std::is_trivially_copy_constructible_v<VoidPtr> &&
			             std::is_trivially_copy_assignable_v<VoidPtr> &&
					     std::is_trivially_destructible_v<VoidPtr> = default;
			// clang-format on

			constexpr node_storage_variant& operator=(node_storage_variant const& other) noexcept
			{
				if (kind_ == other.kind_)
				{
					switch (kind_)
					{
					case kind_t::string:
						str_ = other.str_;
						break;
					case kind_t::array:
						arr_ = other.arr_;
						break;
					case kind_t::object:
						obj_ = other.obj_;
						break;
					case kind_t::number:
						num_ = other.num_;
						break;
					case kind_t::integer:
						int_ = other.int_;
						break;
					case kind_t::uinteger:
						uint_ = other.uint_;
						break;
					}
				}
				else
				{
					destroy_union();
					kind_ = other.kind_;
					copy_replace_union(other);
				}
				return *this;
			}

			// clang-format off
			node_storage_variant& operator=(node_storage_variant&&) noexcept
			    requires std::is_trivially_move_constructible_v<VoidPtr> &&
			             std::is_trivially_move_assignable_v<VoidPtr> &&
					     std::is_trivially_destructible_v<VoidPtr> = default;
			// clang-format on

			constexpr node_storage_variant& operator=(node_storage_variant&& other) noexcept
			{
				if (kind_ == other.kind_)
				{
					switch (kind_)
					{
					case kind_t::string:
						str_ = std::move(other.str_);
						break;
					case kind_t::array:
						arr_ = std::move(other.arr_);
						break;
					case kind_t::object:
						obj_ = std::move(other.obj_);
						break;
					case kind_t::number:
						num_ = other.num_;
						break;
					case kind_t::integer:
						int_ = other.int_;
						break;
					case kind_t::uinteger:
						uint_ = other.uint_;
						break;
					}
				}
				else
				{
					destroy_union();
					kind_ = other.kind_;
					move_replace_union(other);
				}
				return *this;
			}

			// clang-format off
			~node_storage_variant() noexcept requires std::is_trivially_destructible_v<VoidPtr> = default;
			// clang-format on

			constexpr ~node_storage_variant() noexcept
			{
				destroy_union();
			}
		};

#if __has_cpp_attribute(no_unique_address)
#define BIZWEN_NO_UNIQUE_ADDRESS [[no_unique_address]]
#elif __has_cpp_attribute(msvc::no_unique_address)
#define BIZWEN_NO_UNIQUE_ADDRESS [[msvs::no_unique_address]]
#endif

#ifdef BIZWEN_NO_UNIQUE_ADDRESS
		template <typename Number,
		    typename Integer, typename UInteger, typename Allocator>
		struct basic_json_node_storage
		{
			BIZWEN_NO_UNIQUE_ADDRESS Allocator alloc_;
			node_storage_variant<detail::ator_void_ptr_t<Allocator>, Number, Integer, UInteger> var_;

			constexpr Allocator& get_allocator_ref() noexcept
			{
				return alloc_;
			}
			constexpr Allocator const& get_allocator_ref() const noexcept
			{
				return alloc_;
			}
		};
#else
		template <typename Number,
		    typename Integer, typename UInteger, typename Allocator>
		struct basic_json_node_storage
		{
			Allocator alloc_;
			node_storage_variant<detail::ator_void_ptr_t<Allocator>, Number, Integer, UInteger> var_;

			constexpr Allocator& get_allocator_ref() noexcept
			{
				return alloc_;
			}
			constexpr Allocator const& get_allocator_ref() const noexcept
			{
				return alloc_;
			}
		};

		template <typename Number,
		    typename Integer, typename UInteger, typename Allocator>
		    requires std::is_class_v<Allocator> && (!std::is_final_v<Allocator>)
		struct basic_json_node_storage<Number, Integer, UInteger, Allocator>: Allocator
		{
			node_storage_variant<detail::ator_void_ptr_t<Allocator>, Number, Integer, UInteger> var_;

			constexpr Allocator& get_allocator_ref() noexcept
			{
				return (Allocator&)(*this);
			}
			constexpr Allocator const& get_allocator_ref() const noexcept
			{
				return (Allocator const&)(*this);
			}
		};
#endif
#undef BIZWEN_NO_UNIQUE_ADDRESS
	} // namespace detail

	template <typename Number,
	    typename Integer, typename UInteger, typename Allocator>
	class basic_json_node
	{
	public:
		using number_type = Number;
		using integer_type = Integer;
		using uinteger_type = UInteger;
		using allocator_type = Allocator;

	private:
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
		friend constexpr detail::node_ator_rebound_ptr_t<T, Node> detail::alloc_from_node(Node& node);

		template <typename T, typename Node>
		friend constexpr void detail::dealloc_from_node(Node& node, detail::node_ator_rebound_ptr_t<T, Node> p) noexcept;

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

		using stor_var_t = detail::node_storage_variant<detail::ator_void_ptr_t<Allocator>, Number, Integer, UInteger>;
		using kind_t = stor_var_t::kind_t;

		detail::basic_json_node_storage<Number, Integer, UInteger, Allocator> stor_;

		constexpr Allocator& get_allocator_ref() noexcept
		{
			return stor_.get_allocator_ref();
		}
		constexpr const Allocator& get_allocator_ref() const noexcept
		{
			return stor_.get_allocator_ref();
		}

		constexpr kind_t kind() const noexcept
		{
			return stor_.var_.kind_;
		}

		constexpr stor_var_t& stor_var() noexcept
		{
			return stor_.var_;
		}
		constexpr const stor_var_t& stor_var() const noexcept
		{
			return stor_.var_;
		}

	public:
		// clang-format off
		constexpr basic_json_node() requires std::default_initializable<Allocator> = default;
		// clang-format on

		constexpr explicit basic_json_node(Allocator const& a) noexcept
		    : stor_{ { a } }
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
			using stor_var_t = node_type::stor_var_t;

			using str_ptr_t = node_ator_rebound_ptr_t<string_type, node_type>;
			using arr_ptr_t = node_ator_rebound_ptr_t<array_type, node_type>;
			using obj_ptr_t = node_ator_rebound_ptr_t<object_type, node_type>;

			friend json_type;

			constexpr kind_t kind() const
			{
				// assert(node_);
				// Lakos rule
				if (!node_)
					throw json_error(json_errc::is_empty);

				return node_->kind();
			}

			constexpr stor_var_t const& stor_var() const noexcept
			{
				return node_->stor_var();
			}

		public:
			[[nodiscard]] constexpr bool empty() const noexcept
			{
				return node_ != nullptr;
			}

			[[nodiscard]] constexpr bool undefined() const noexcept
			{
				return kind() == kind_t::undefined;
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
				auto s = stor_var();

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

				return *static_cast<str_ptr_t>(stor_var().str_);
			}

			constexpr explicit operator array_type const&() const&
			{
				if (!array())
					throw json_error(json_errc::not_array);

				return *static_cast<arr_ptr_t>(stor_var().arr_);
			}

			constexpr explicit operator object_type const&() const&
			{
				if (!object())
					throw json_error(json_errc::not_object);

				return *static_cast<obj_ptr_t>(stor_var().obj_);
			}

			constexpr explicit operator integer_type() const
			    requires HasInteger
			{
				if (!integer())
					throw json_error(json_errc::not_integer);

				return stor_var().int_;
			}

			constexpr explicit operator uinteger_type() const
			    requires HasUInteger
			{
				if (!uinteger())
					throw json_error(json_errc::not_uinteger);

				return stor_var().uint_;
			}

			// defined after the class body of basic_const_json_slice

			constexpr auto operator[](key_string_type const& k) const -> const_slice_type;

			template <typename KeyStrLike>
			    requires transparently_comparable_json_object<Object>
			    && noncovertible_to_key_char_cptr<KeyStrLike, Object>
			constexpr auto operator[](KeyStrLike const& k) const -> const_slice_type;

			constexpr auto operator[](key_char_type const* k) const -> const_slice_type;

			template <std::integral T>
			constexpr auto operator[](T pos) const -> const_slice_type;

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
		template <typename Node, typename String,
		    typename Array,
		    typename Object,
		    bool HasInteger, bool HasUInteger>
		constexpr auto basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>::operator[](key_string_type const& k) const -> const_slice_type
		{
			if (!object())
				throw json_error(json_errc::nonobject_indexing);

			auto const& o = *static_cast<obj_ptr_t>(stor_var().obj_);
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

			auto const& o = *static_cast<obj_ptr_t>(stor_var().obj_);
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

			auto const& o = *static_cast<obj_ptr_t>(stor_var().obj_);
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
		template <std::integral T>
		constexpr auto basic_json_slice_common_base<Node, String, Array, Object, HasInteger, HasUInteger>::operator[](T pos) const -> const_slice_type
		{
			if (!array())
				throw json_error(json_errc::nonarray_indexing);

			auto const& a = *static_cast<arr_ptr_t>(stor_var().arr_);

			return a[pos];
		}

		template <typename T, typename Node>
		constexpr node_ator_rebound_ptr_t<T, Node> alloc_from_node(Node& node)
		{
			using ator_t = std::allocator_traits<typename Node::allocator_type>::template rebind_alloc<T>;
			return ator_t(node.get_allocator_ref()).allocate(1);
		}

		template <typename T, typename Node>
		constexpr void dealloc_from_node(Node& node, node_ator_rebound_ptr_t<T, Node> p) noexcept
		{
			using ator_t = std::allocator_traits<typename Node::allocator_type>::template rebind_alloc<T>;
			return ator_t(node.get_allocator_ref()).deallocate(p, 1);
		}

		template <typename T, typename Node>
		struct node_variant_alloc_guard
		{
			node_ator_rebound_ptr_t<T, Node> ptr;
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

			constexpr node_ator_rebound_ptr_t<T, Node> get() noexcept
			{
				assert(ptr);

				return ptr;
			}

			constexpr ~node_variant_alloc_guard()
			{
				if (ptr)
					detail::dealloc_from_node<T>(node, ptr);
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

		using str_ptr_t = detail::node_ator_rebound_ptr_t<typename base_type::string_type, Node>;
		using arr_ptr_t = detail::node_ator_rebound_ptr_t<typename base_type::array_type, Node>;
		using obj_ptr_t = detail::node_ator_rebound_ptr_t<typename base_type::object_type, Node>;

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

		friend constexpr void swap(basic_json_slice& lhs, basic_json_slice& rhs) noexcept
		{
			lhs.swap(rhs);
		}

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

			return *static_cast<str_ptr_t>(stor_var().str_);
		}

		constexpr explicit operator array_type&() &
		{
			if (!array())
				throw json_error(json_errc::not_array);

			return *static_cast<arr_ptr_t>(stor_var().arr_);
		}

		constexpr explicit operator object_type&() &
		{
			if (!object())
				throw json_error(json_errc::not_object);

			return *static_cast<obj_ptr_t>(stor_var().obj_);
		}

	private:
		using kind_t = node_type::kind_t;
		using stor_var_t = node_type::stor_var_t;

		using string_ator = std::allocator_traits<allocator_type>::template rebind_alloc<String>;
		using string_ator_traits = std::allocator_traits<string_ator>;
		using object_ator = std::allocator_traits<allocator_type>::template rebind_alloc<Object>;
		using object_ator_traits = std::allocator_traits<object_ator>;

		constexpr void allocate_object()
		{
			detail::node_variant_alloc_guard<object_type, node_type> guard(*node_);
			auto ator = object_ator(node_->get_allocator_ref());
			auto rawptr = guard.get();
			object_ator_traits::construct(ator, std::to_address(rawptr));
			node_->stor_var().set_obj_ptr(rawptr);
			guard.release();
		}

		constexpr stor_var_t& stor_var() noexcept
		{
			return node_->stor_var();
		}

	public:
		using base_type::operator[];

		constexpr void reset() noexcept
		{
			json_type::reset_node(*node_);
		}

		constexpr basic_json_slice operator[](key_string_type const& k)
		{
			auto e = undefined();

			if (!object() && !e)
				throw json_error(json_errc::nonobject_indexing);

			if (e)
				allocate_object();

			auto& o = *static_cast<obj_ptr_t>(stor_var().obj_);
			auto [i, _] = o.emplace(k, node_type{ node_->get_allocator_ref() });
			auto& [_, v] = *i;

			return v;
		}

		template <typename KeyStrLike>
		    requires detail::transparently_comparable_json_object<Object>
		    && detail::noncovertible_to_key_char_cptr<KeyStrLike, Object>
		constexpr basic_json_slice operator[](KeyStrLike const& k)
		{
			auto e = undefined();

			if (!object() && !e)
				throw json_error(json_errc::nonobject_indexing);

			if (e)
				allocate_object();

			auto& o = *static_cast<obj_ptr_t>(stor_var().obj_);
			auto [i, _] = o.emplace(k, node_type{ node_->get_allocator_ref() });
			auto& [_, v] = *i;

			return v;
		}

		constexpr basic_json_slice operator[](key_char_type const* k)
		{
			auto e = undefined();

			if (!object() && !e)
				throw json_error(json_errc::nonobject_indexing);

			if (e)
				allocate_object();

			auto& o = *static_cast<obj_ptr_t>(stor_var().obj_);
			auto [i, _] = o.emplace(k, node_type{ node_->get_allocator_ref() });
			auto& [_, v] = *i;

			return v;
		}

		template <std::integral T>
		constexpr basic_json_slice operator[](T pos)
		{
			if (!array())
				throw json_error(json_errc::nonarray_indexing);

			auto& a = *static_cast<arr_ptr_t>(stor_var().arr_);

			return a[pos];
		}

		constexpr basic_json_slice& operator=(string_type const& str)
		{
			bool is_string = string();
			bool is_undefined = undefined();

			if (!is_string && !is_undefined)
				throw json_error(json_errc::not_undefined_or_string);

			if (is_string)
			{
				*static_cast<str_ptr_t>(stor_var().str_) = str;
			}
			else // undefined
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, std::to_address(rawptr), str);
				node_->stor_var().set_str_ptr(rawptr);
				guard.release();
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(string_type&& str)
		{
			bool is_string = string();
			bool is_undefined = undefined();

			if (!is_string && !is_undefined)
				throw json_error(json_errc::not_undefined_or_string);

			if (is_string)
			{
				*static_cast<str_ptr_t>(stor_var().str_) = std::move(str);
			}
			else // undefined
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, std::to_address(rawptr), std::move(str));
				node_->stor_var().set_str_ptr(rawptr);
				guard.release();
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(char_type const* str)
		{
			bool is_string = string();
			bool is_undefined = undefined();

			if (!is_string && !is_undefined)
				throw json_error(json_errc::not_undefined_or_string);

			if (is_string)
			{
				*static_cast<str_ptr_t>(stor_var().str_) = str;
			}
			else // undefined
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, std::to_address(rawptr), str);
				node_->stor_var().set_str_ptr(rawptr);
				guard.release();
			}

			return *this;
		}

		template <typename StrLike>
		    requires std::constructible_from<string_type, StrLike>
		    && (!std::is_convertible_v<StrLike const&, char_type const*>)
		constexpr basic_json_slice& operator=(StrLike const& str)
		{
			bool is_string = string();
			bool is_undefined = undefined();

			if (!is_string && !is_undefined)
				throw json_error(json_errc::not_undefined_or_string);

			if (is_string)
			{
				*static_cast<str_ptr_t>(stor_var().str_) = str;
			}
			else
			{
				detail::node_variant_alloc_guard<string_type, node_type> guard(*node_);
				auto ator = string_ator(node_->get_allocator_ref());
				auto rawptr = guard.get();
				string_ator_traits::construct(ator, std::to_address(rawptr), str);
				node_->stor_var().set_str_ptr(rawptr);
				guard.release();
			}

			return *this;
		}

		constexpr basic_json_slice& operator=(nulljson_t)
		{
			bool is_null = null();
			bool is_undefined = undefined();

			if (!is_null && !is_undefined)
				throw json_error(json_errc::not_undefined_or_null);

			if (is_undefined)
				node_->stor_var().set_null();

			return *this;
		}

		template <typename T>
		    requires std::is_arithmetic_v<T>
		constexpr basic_json_slice& operator=(T n)
		{
			if constexpr (std::same_as<T, bool>)
			{
				bool is_boolean = boolean();
				bool is_undefined = undefined();

				if (!is_boolean && !is_undefined)
					throw json_error(json_errc::not_undefined_or_boolean);

				if (is_undefined)
					node_->stor_var().set_bool(n);
			}
			else if constexpr (HasInteger && std::signed_integral<T>)
			{
				bool is_number = number() || uinteger() || integer();
				bool is_undefined = undefined();

				if (!is_number && !is_undefined)
					throw json_error(json_errc::not_undefined_or_number);

				node_->stor_var().set_int(n);
			}
			else if constexpr (HasUInteger && std::unsigned_integral<T>)
			{
				bool is_number = number() || uinteger() || integer();
				bool is_undefined = undefined();

				if (!is_number && !is_undefined)
					throw json_error(json_errc::not_undefined_or_number);

				node_->stor_var().set_uint(n);
			}
			else // fallback
			{
				bool is_number = number() || uinteger() || integer();
				bool is_undefined = undefined();

				if (!is_number && !is_undefined)
					throw json_error(json_errc::not_undefined_or_number);

				node_->stor_var().set_num(n);
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
			constexpr auto node_to_slice = [](node_type& node) static noexcept {
				return basic_json_slice{ node };
			};
			return static_cast<array_type&>(*this) | std::views::transform(node_to_slice);
		}

		using base_type::as_object;

		constexpr auto as_object()
		{
			constexpr auto pair_node_to_slice = [](map_node_type& pair) static noexcept {
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
		using stor_var_t = node_type::stor_var_t;

		using str_ptr_t = const_slice_type::str_ptr_t;
		using arr_ptr_t = const_slice_type::arr_ptr_t;
		using obj_ptr_t = const_slice_type::obj_ptr_t;

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

		static constexpr void reset_node(node_type& node) noexcept
		{
			auto& s = node.stor_var();

			switch (node.kind())
			{
			case kind_t::string: {
				auto p = static_cast<str_ptr_t>(s.str_);
				auto ator = string_ator(node.get_allocator_ref());
				string_ator_traits::destroy(ator, p);
				detail::dealloc_from_node<string_type>(node, p);

				break;
			}
			case kind_t::array: {
				auto p = static_cast<arr_ptr_t>(s.arr_);

				for (auto&& i : *p)
				{
					auto&& json = basic_json(std::move(i));
					json.reset();
				}

				auto ator = array_ator(node.get_allocator_ref());
				array_ator_traits::destroy(ator, p);
				detail::dealloc_from_node<array_type>(node, p);

				break;
			}
			case kind_t::object: {
				auto p = static_cast<obj_ptr_t>(s.obj_);

				for (auto&& [_, v] : *p)
				{
					auto&& json = basic_json(std::move(v));
					json.reset();
				}

				auto ator = object_ator(node.get_allocator_ref());
				object_ator_traits::destroy(ator, p);
				detail::dealloc_from_node<object_type>(node, p);

				break;
			}
			default: {
				// make clang happy
			}
			}

			s.destroy_union();
			s.kind_ = kind_t{};
		}

		constexpr void reset() noexcept
		{
			reset_node(node_);
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
					basic_json::reset_node(*begin);
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
					basic_json::reset_node(*begin);
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
					basic_json::reset_node(value);
				}
			}
		};

		constexpr void swap_without_ator(basic_json& rhs) noexcept
		{
			auto temp = std::move(node_.stor_var());
			node_.stor_var() = std::move(rhs.node_.stor_var());
			rhs.node_.stor_var() = std::move(temp);
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
					rhs.reset();
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
				reset();
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
					reset();
					clone(rhs);
					rhs.reset();
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
				node_.stor_var().set_bool(n);
			}
			else if constexpr (HasInteger && std::signed_integral<T>)
			{
				node_.stor_var().set_int(n);
			}
			else if constexpr (HasUInteger && std::unsigned_integral<T>)
			{
				node_.stor_var().set_uint(n);
			}
			else // fallback
			{
				node_.stor_var().set_num(n);
			}
		}

		constexpr explicit basic_json(string_type v, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, std::to_address(rawptr), std::move(v));
			node_.set_str_ptr(rawptr);
			guard.release();
		}

		constexpr basic_json(char_type const* begin, char_type const* end, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, std::to_address(rawptr), begin, end);
			node_.stor_var().set_str_ptr(rawptr);
			guard.release();
		}

		constexpr basic_json(char_type const* str, string_type::size_type count, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, std::to_address(rawptr), str, count);
			node_.stor_var().set_str_ptr(rawptr);
			guard.release();
		}

		constexpr explicit basic_json(char_type const* str, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			detail::node_variant_alloc_guard<string_type, node_type> guard(node_);
			auto ator = string_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			string_ator_traits::construct(ator, std::to_address(rawptr), str);
			node_.stor_var().set_str_ptr(rawptr);
			guard.release();
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
			string_ator_traits::construct(ator, std::to_address(rawptr), str);
			node_.stor_var().set_str_ptr(rawptr);
			guard.release();
		}

		constexpr basic_json(array_type arr, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			rollbacker_array_all_ rollbacker(arr);
			detail::node_variant_alloc_guard<array_type, node_type> guard(node_);
			auto ator = array_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			array_ator_traits::construct(ator, std::to_address(rawptr), std::move(arr));
			node_.stor_var().set_arr_ptr(rawptr);
			guard.release();
			rollbacker.release();
		}

		constexpr basic_json(object_type obj, allocator_type const& a = allocator_type())
		    : node_(a)
		{
			rollbacker_map_all_ rollbacker(obj);
			detail::node_variant_alloc_guard<object_type, node_type> guard(node_);
			auto ator = object_ator(node_.get_allocator_ref());
			auto rawptr = guard.get();
			object_ator_traits::construct(ator, std::to_address(rawptr), std::move(obj));
			node_.stor_var().set_obj_ptr(rawptr);
			guard.release();
			rollbacker.release();
		}

		constexpr basic_json(node_type&& n) noexcept
		    : node_(std::move(n))
		{
			n.stor_var() = stor_var_t{};
		}

		constexpr basic_json(node_type&& n, allocator_type const& a) noexcept
		    : node_(a)
		{
			node_.stor_var() = std::exchange(n.stor_var(), {});
		}

		constexpr allocator_type get_allocator() const noexcept
		{
			return node_.get_allocator_ref();
		}

		[[nodiscard("discard nodes will cause leaks")]] constexpr operator node_type() && noexcept
		{
			auto node = node_;
			node_.stor_var() = {};
			return node;
		}

	private:
		static constexpr void clone_node(node_type& lhs, node_type const& rhs)
		{
			auto rk = rhs.kind();
			auto const& rs = rhs.stor_var();
			auto& s = lhs.stor_var();

			switch (rk)
			{
			case kind_t::string: {
				auto const& rstr = *static_cast<str_ptr_t>(rs.str_);
				detail::node_variant_alloc_guard<string_type, node_type> mem(lhs);
				auto ator = string_ator(lhs.get_allocator_ref());
				auto lptr = mem.get();
				string_ator_traits::construct(ator, std::to_address(lptr), rstr);
				mem.release();
				lhs.stor_var().set_str_ptr(lptr);
				break;
			}
			case kind_t::array: {
				if constexpr (std::is_trivially_copyable_v<allocator_type>)
				{
					auto const& rarr = *static_cast<arr_ptr_t>(rs.arr_);
					detail::node_variant_alloc_guard<array_type, node_type> guard(lhs);
					auto ator = array_ator(lhs.get_allocator_ref());
					auto lptr = guard.get();
					array_ator_traits::construct(ator, std::to_address(lptr), rarr);
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
					s.set_arr_ptr(lptr);
				}
				else
				{
					auto const& rarr = *static_cast<arr_ptr_t>(rs.arr_);
					detail::node_variant_alloc_guard<array_type, node_type> guard(lhs);
					auto ator = array_ator(lhs.get_allocator_ref());
					auto lptr = guard.get();
					array_ator_traits::construct(ator, std::to_address(lptr));
					array_type& larr{ *lptr };
					larr.reserve(rarr.size());
					rollbacker_array_all_ rollbacker(larr);

					auto first = rarr.begin();
					auto last = rarr.end();
					for (; first != last; ++first)
					{
						basic_json temp{ lhs.get_allocator_ref() };
						clone_node(temp.node_, *first);
						larr.push_back(std::move(temp));
					}

					guard.release();
					s.set_arr_ptr(lptr);
				}
				break;
			}
			case kind_t::object: {
				auto const& robj = *static_cast<obj_ptr_t>(rs.obj_);
				detail::node_variant_alloc_guard<object_type, node_type> guard(lhs);
				auto ator = object_ator(lhs.get_allocator_ref());
				auto lptr = guard.get();
				object_ator_traits::construct(ator, std::to_address(lptr));
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
				lhs.stor_.var_.set_obj_ptr(lptr);
				break;
			}
			default: {
				s = rs;
			}
			}
		}

		constexpr void clone(const basic_json& rhs)
		{
			clone_node(node_, rhs.node_);
		}

	public:
		constexpr ~basic_json() noexcept
		{
			reset();
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
		treat_undefined_as_null = 0x200,
		treat_undefined_as_undefined = 0x400,
		treat_undefined_as_literal = 0x800, // debug only
	};

	using json = basic_json<>;
	using json_node = basic_json_node<>;
	using json_slice = basic_json_slice<>;
	using const_json_slice = basic_const_json_slice<>;

	namespace pmr
	{
		template <typename Number = double, typename Integer = long long, typename UInteger = unsigned long long>
		using basic_json_node = bizwen::basic_json_node<Number, Integer, UInteger, std::pmr::polymorphic_allocator<>>;

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
		using json_node = basic_json_node<>;
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
