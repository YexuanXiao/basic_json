#include <string>
#include <map>
#include <memory>
#include <vector>
// https://www.rfc-editor.org/rfc/rfc8259

namespace bizwen
{

    template <typename Boolean = bool, typename Number = double, typename Integer = long long, typename UInteger = unsigned long long>
    struct json_handle
    {
        using boolean_t = Boolean;
        using number_t = Number;
        using integer_t = Integer;
        using uinteger_t = UInteger;

        enum class kind_t : unsigned char
        {
            null,
            boolean,
            number,
            string,
            array,
            object,
            integer,
            uinteger
        };

        union storage_type_
        {
            decltype(nullptr) null_;
            boolean_t bool_;
            number_t num_;
            integer_t int_;
            uinteger_t uint_;
            void* ptr_;
        };

        storage_type_ stor_;
        kind_t kind_;
    };

    template <typename Handle = json_handle<>, typename String = std::string, typename Array = std::vector<json_handle<>>, typename Map = std::map<std::string, json_handle<>>, typename Allocator = std::allocator<json_handle<>>, bool has_integer = true, bool has_uinteger = true>
    class basic_json
    {
        using handle_t = Handle;
        using number_t = handle_t::number_t;
        using boolean_t = handle_t::boolean_t;
        using integer_t = handle_t::integer_t;
        using uinteger_t = handle_t::uinteger_t;
        using null_t = decltype(nullptr);
        using string_t = String;
        using object_t = Map;
        using array_t = Array;
        using kind_t = handle_t::kind_t;

        handle_t handle_;

        void allocate(kind_t k)
        {
            handle_.kind_ = k;
            auto&& stor_ = handle_.stor_;

            switch (k)
            {
            case kind_t::null: {
                stor_.null_ = nullptr;
                break;
            }
            case kind_t::boolean: {
                stor_.bool_ = boolean_t{};
                break;
            }
            case kind_t::number: {
                stor_.num_ = number_t{};
                break;
            }
            case kind_t::string: {
                stor_.str_ = new string_t{};
                break;
            }
            case kind_t::array: {
                stor_.arr_ = new array_t{};
                break;
            }
            case kind_t::object: {
                stor_.obj_ = new object_t{};
                break;
            }
            case kind_t::integer: {
                stor_.int_ = integer_t{};
                break;
            }
            case kind_t::uinteger: {
                stor_.uint_ = uinteger_t{};
                break;
            }
            }
        }

        void dealloc(kind_t k)
        {
            handle_.kind_ = k;
            auto&& stor_ = handle_.stor_;

            switch (k)
            {
            case kind_t::string: {
                delete (stor_.ptr_);
                break;
            }
            case kind_t::array: {
                delete (stor_.ptr_);
                break;
            }
            case kind_t::object: {
                delete (stor_.ptr_);
                break;
            }
            }
        }
    };

    using json = basic_json<>;

}
