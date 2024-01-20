#include <cstddef>
#include <cstdint>
#include <optional>
#include <utility>
#include <string>
#include <map>
#include <memory>
#include <variant>
#include <vector>
#include <stdfloat>
#include <type_traits>
#include <limits>

// https://www.rfc-editor.org/rfc/rfc8259

namespace bizwen
{

    template <template <typename...> class Map, template <typename...> class Array, template <typename...> class Allocator>
    concept flat_map_pattern = requires { typename Map<int, int, std::less<int>, Array<int, Allocator<int>>, Array<int, Allocator<int>>>; };
    template <template <typename...> class Map, template <typename...> class Allocator>
    concept map_pattern = requires { typename Map<int, int, std::less<int>, Allocator<std::pair<const int, int>>>; };

#if __STDCPP_FLOAT64_T__ != 1
#define BIZWEN_JSON_NUMBER_TYPE double
#else
#define BIZWEN_JSON_NUMBER_TYPE std::float64_t
#endif

    // clang-format off
    template <class String = std::string,
        class Number = std::conditional_t<std::numeric_limits<double>::is_iec559, double, BIZWEN_JSON_NUMBER_TYPE>,
        template <typename...> class Object = std::map,
        template <typename...> class Array = std::vector,
        class Boolean = bool,
        class Int64 = long long,
        class UInt64 = unsigned long long,
        template <typename...> class Allocator = std::allocator>
    // clang-format on
    class basic_json
    {
        static auto object_type_helper()
        {
            if constexpr (flat_map_pattern<Object, Array, Allocator>)
                return Object<String, basic_json, std::less<>, Allocator<std::pair<String, basic_json>>>();
            else if constexpr (map_pattern<Object, Allocator>)
                return Object<String, basic_json, std::less<>, Array<String, Allocator<String>>, Array<basic_json, Allocator<String>>>();
            else
                static_assert(false);
        }

        using string_t = String;
        using number_t = Number;
        using object_t = decltype(object_type_helper());
        using array_t = Array<basic_json, Allocator<basic_json>>;
        using boolean_t = Boolean;
        using int64_t = Int64;
        using uint64_t = UInt64;
        using null_t = decltype(nullptr);

        enum class value_t : unsigned char
        {
            null,
            boolean,
            number,
            string,
            array,
            object,
            int64,
            uint64
        };

        union storage_type_
        {
            null_t null_;
            boolean_t bool_;
            number_t num_;
            string_t* str_;
            array_t* arr_;
            object_t* obj_;
            int64_t int64_;
            uint64_t uint64_;

            void allocate(value_t v_t)
            {
                using enum value_t;
                switch (v_t)
                {
                case null: {
                    null_ = decltype(null_){};
                    break;
                }
                case boolean: {
                    bool_ = decltype(bool_){};
                    break;
                }
                case number: {
                    num_ = decltype(num_){};
                    break;
                }
                case string: {
                    str_ = new decltype(str_){};
                    break;
                }
                case array: {
                    arr_ = new decltype(arr_){};
                    break;
                }
                case object: {
                    obj_ = new decltype(obj_){};
                    break;
                }
                case int64: {
                    int64_ = decltype(int64_){};
                    break;
                }
                case uint64: {
                    uint64_ = decltype(uint64_){};
                    break;
                }
                }
            }

            void dealloc(value_t v_t)
            {
                using enum value_t;
                switch (v_t)
                {
                case string: {
                    delete (str_);
                    break;
                }
                case array: {
                    delete (arr_);
                    break;
                }
                case object: {
                    delete (obj_);
                    break;
                }
                }
            }
        };

        storage_type_ stor_{};
    };
}

struct json_handle
{
    void* ptr_;
};

template <typename String, typename Array, typename Map, bool has_integer, bool has_uinteger, typename Allocator>
class basic_json
{
    enum class value_t : unsigned char
    {
        null,
        boolean,
        number,
        string,
        array,
        object,
        int64,
        uint64
    };

        using string_t = String;
        using number_t = double;
        using object_t = Map;
        using array_t = Array;
        using boolean_t = bool;
        using int64_t = std::int64_t;
        using uint64_t = int64_t;
        using null_t = std::nullopt_t;
    
    using erased_type = std::variant<std::nullopt_t, bool, double, String, Array, Map, basic_json, std::int64_t, std::uint64_t>;
    void* storptr_;
};

using json = basic_json<std::string, std::vector<json_handle>, std::map<std::string, json_handle>, true, true, std::allocator<json_handle>>;