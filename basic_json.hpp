#include <cassert>
#include <iterator>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <concepts>

// https://www.rfc-editor.org/rfc/rfc8259

namespace bizwen
{
    struct nulljson_t
    {
        explicit constexpr nulljson_t(decltype(nullptr)) {}
    };

    inline constexpr nulljson_t nulljson{ nullptr };

    template <typename Boolean = bool, typename Number = double,
        typename Integer = long long, typename UInteger = unsigned long long>
    struct json_node
    {
        using boolean_t = Boolean;
        using number_t = Number;
        using integer_t = Integer;
        using uinteger_t = UInteger;

        static_assert(std::floating_point<number_t>);
        static_assert(std::signed_integral<integer_t>);
        static_assert(std::unsigned_integral<uinteger_t>);
        static_assert(std::integral<boolean_t>);

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

        union storage_type_
        {
            void* str_;
            void* arr_;
            void* obj_;
            number_t num_;
            integer_t int_;
            uinteger_t uint_;
        };

        storage_type_ stor_{};
        kind_t kind_{};

        constexpr json_node() noexcept = default;

        constexpr json_node(json_node const&) = delete;

        constexpr json_node(json_node&& rhs) noexcept
        {
            {
                auto temp = stor_;
                stor_ = rhs.stor_;
                rhs.stor_ = temp;
            }
            {
                auto temp = kind_;
                kind_ = rhs.kind_;
                rhs.kind_ = temp;
            }
        }
    };

    template <typename node = json_node<>, typename String = std::string,
        typename Array = std::vector<json_node<>>,
        typename Map = std::map<std::string, json_node<>>,
        typename Allocator = std::allocator<json_node<>>,
        bool has_integer = true, bool has_uinteger = true>
    struct basic_json;

    template <typename Node = json_node<>, typename String = std::string,
        typename Array = std::vector<json_node<>>,
        typename Map = std::map<std::string, json_node<>>,
        typename Allocator = std::allocator<json_node<>>,
        bool has_integer = true, bool has_uinteger = true>
    struct basic_const_json_span
    {
        using node_t = Node;
        using number_t = node_t::number_t;
        using boolean_t = node_t::boolean_t;
        using integer_t = node_t::integer_t;
        using uinteger_t = node_t::uinteger_t;
        using string_t = String;
        using object_t = Map;
        using array_t = Array;
        using allocator_t = Allocator;
        using kind_t = node_t::kind_t;
        using json_t = basic_json<node_t, string_t, array_t, object_t, allocator_t, has_integer, has_uinteger>;

        static_assert(std::floating_point<number_t>);
        static_assert(std::signed_integral<integer_t>);
        static_assert(std::unsigned_integral<uinteger_t>);
        static_assert(std::integral<boolean_t>);
        static_assert(std::same_as<json_node<boolean_t, number_t, integer_t, uinteger_t>, node_t>);
        static_assert(std::random_access_iterator<typename string_t::iterator>);
        static_assert(std::random_access_iterator<typename array_t::iterator>);
        static_assert(std::bidirectional_iterator<typename object_t::iterator>);
        static_assert(std::integral<typename string_t::value_type>);
        static_assert(std::same_as<node_t, typename array_t::value_type>);
        static_assert(std::same_as<node_t, typename object_t::mapped_type>);
        static_assert(std::same_as<string_t, typename object_t::key_type>);

        json_t* json_{};

        [[nodiscard]] constexpr kind_t kind() const noexcept
        {
            return json_->node_.kind_;
        }

        [[nodiscard]] constexpr node_t::storage_type_& stor() noexcept
        {
            return json_->node_.stor_;
        }

        [[nodiscard]] constexpr node_t::storage_type_ const& stor() const noexcept
        {
            return json_->node_.stor_;
        }

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
            return kind() == kind_t::number;
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
            requires(has_integer)
        {
            return kind() == kind_t::integer;
        }

        [[nodiscard]] constexpr bool uinteger() const noexcept
            requires(has_uinteger)
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

        basic_const_json_span(node_t const& n) noexcept
            : json_(reinterpret_cast<json_t*>(n))
        {
        }

        constexpr explicit operator boolean_t() const noexcept
        {
            // todo: replace all asserts as exceptions
            assert(boolean());
            auto k = kind();
            return k == kind_t::true_value ? 1 : 0;
        }

        constexpr explicit operator number_t() const noexcept
        {
            assert(number());
            return json_->stor_.num_;
        }

        constexpr explicit operator nulljson_t() const noexcept
        {
            assert(null());
            return nulljson;
        }

        constexpr explicit operator const string_t&() const& noexcept
        {
            assert(string());
            return *(json_->stor_.str_);
        }

        constexpr explicit operator const array_t&() const& noexcept
        {
            assert(array());
            return *(json_->stor_.arr_);
        }

        constexpr explicit operator const object_t&() const& noexcept
        {
            assert(object());
            return *(json_->stor_.obj_);
        }

        constexpr explicit operator integer_t() const noexcept
            requires(has_integer)
        {
            assert(integer());
            return json_->stor_.int_;
        }

        constexpr explicit operator uinteger_t() const noexcept
            requires(has_uinteger)
        {
            assert(uinteger());
            return json_->stor_.uint_;
        }
    };

    template <typename node, typename String,
        typename Array,
        typename Map,
        typename Allocator,
        bool has_integer, bool has_uinteger>
    struct basic_json
    {
        using node_t = node;
        using number_t = node_t::number_t;
        using boolean_t = node_t::boolean_t;
        using integer_t = node_t::integer_t;
        using uinteger_t = node_t::uinteger_t;
        using null_t = nulljson_t;
        using string_t = String;
        using object_t = Map;
        using array_t = Array;
        using allocator_t = Allocator;
        using kind_t = node_t::kind_t;

        static_assert(std::floating_point<number_t>);
        static_assert(std::signed_integral<integer_t>);
        static_assert(std::unsigned_integral<uinteger_t>);
        static_assert(std::integral<boolean_t>);
        static_assert(std::same_as<json_node<boolean_t, number_t, integer_t, uinteger_t>, node_t>);
        static_assert(std::random_access_iterator<typename string_t::iterator>);
        static_assert(std::random_access_iterator<typename array_t::iterator>);
        static_assert(std::bidirectional_iterator<typename object_t::iterator>);
        static_assert(std::integral<typename string_t::value_type>);
        static_assert(std::same_as<node_t, typename array_t::value_type>);
        static_assert(std::same_as<node_t, typename object_t::mapped_type>);
        static_assert(std::same_as<string_t, typename object_t::key_type>);

        node_t node_{};

        void dealloc() noexcept
        {
            auto k = node_.kind_;
            auto&& stor_ = node_.stor_;

            switch (k)
            {
            case kind_t::string: {
                delete (stor_.str_);
                break;
            }
            case kind_t::array: {
                delete (stor_.arr_);
                break;
            }
            case kind_t::object: {
                delete (stor_.obj_);
                break;
            }
            }
        }

        constexpr void kind(kind_t k) noexcept { node_.kind_ = k; }

        [[nodiscard]] constexpr kind_t kind() const noexcept
        {
            return node_.kind_;
        }

        [[nodiscard]] constexpr node_t::storage_type_& stor() noexcept
        {
            return node_.stor_;
        }

        [[nodiscard]] constexpr node_t::storage_type_ const& stor() const noexcept
        {
            return node_.stor_;
        }

        constexpr void swap(basic_json& rhs) noexcept
        {
            auto temp = node_;
            node_ = rhs.node_;
            rhs.node_ = temp;
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
        constexpr basic_json(decltype(nullptr)) = delete; // prevent implicit construct string

        constexpr basic_json(boolean_t v) noexcept
        {
            stor().bool_ = v;
            kind(kind_t::boolean);
        }

        constexpr basic_json(string_t v) noexcept
        {
            stor().str_ = new string_t(std::move(v));
            kind(kind_t::string);
        }

        constexpr basic_json(number_t v) noexcept
        {
            stor().num_ = v;
            kind(kind_t::number);
        }

        constexpr basic_json(integer_t v) noexcept
            requires(has_integer)
        {
            stor().int_ = v;
            kind(kind_t::integer);
        }

        constexpr basic_json(uinteger_t v) noexcept
            requires(has_uinteger)
        {
            stor().uint_ = v;
            kind(kind_t::integer);
        }

        constexpr basic_json(node_t&& n) noexcept
        {
            // todo: destory this
            node_ = node_t{};
            node_ = std::move(n);
        }

        constexpr operator node_t() && noexcept
        {
            auto temp = node_;
            node_ = node_t{};
            return temp;
        }
    };

    using json = basic_json<>;

} // namespace bizwen
