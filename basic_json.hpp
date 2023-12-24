#include <utility>
#include <string>
#include <map>
#include <memory>
#include <stdfloat>
#include <type_traits>
#include <limits>

// https://www.rfc-editor.org/rfc/rfc8259

namespace bizwen
{

#if __STDCPP_FLOAT64_T__ != 1
#define BIZWEN_JSON_NUMBER_TYPE double
#else
#define BIZWEN_JSON_NUMBER_TYPE std::float64_t
#endif

    // clang-format off
    template <class String = std::string,
        class Number = std::conditional_t<std::numeric_limits<double>::is_iec559, double, BIZWEN_JSON_NUMBER_TYPE>,
        template <typename U, typename V, typename... Args> class Object = std::map,
        template <typename U, typename... Args> class Array,
        class Boolean = bool,
        class Int64 = long long,
        class UInt64 = unsigned long long,
        template <typename U> class Allocator = std::allocator>
    // clang-format on
    class basic_json
    {
        using string_t = String;
        using number_t = Number;
        using object_t = Object<String, basic_json, std::less<>, Allocator<std::pair<String, basic_json>>>;
        using array_t = Array < basic_json, Allocator<basic_json>;
        using boolean_t = Boolean;
        using int64_t = Int64;
        using uint64_t = UInt64;
        using allocator = Allocator;

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
    };
}
