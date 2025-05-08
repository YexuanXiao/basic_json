#include "basic_json.hpp"
#include <string_view>
#include <cassert>

struct simple_json_node
{
	using variant_type = std::variant<std::monostate, bizwen::nulljson_t, bool, double, long long, unsigned long long,
	    std::string*, std::vector<simple_json_node>*, std::map<std::string, simple_json_node>*>;
	using allocator_type = std::allocator<std::byte>;

	variant_type stor;
	allocator_type alloc;
};

int main()
{
	using node = simple_json_node;
	using json = bizwen::basic_json<node>;
	using slice = bizwen::basic_json_slice<node>;
	using const_slice = bizwen::basic_const_json_slice<node>;
	auto null = bizwen::nulljson;
	using namespace std::literals;
	// A json object represents a json value. The default constructed json object does not hold any value, and its state is "undefined".
	json j01{};
	// Construct a json value with status "number" and value `1`.
	json j02{ 1. };
	// Construct a json value with status "true_value" and value `true`.
	json j03{ true };
	// Construct a json value with status "uinteger" and value `1`. This method is used to accurately store integers with more than 52 bits.
	json j04{ 1ull };
	// Construct a json value with status "string" and value `"abcdef"`.
	json j05{ "abcdef" };
	// Construct json with nullptr is forbidden because json does not have a pointer type and nullptr does not represent the empty string.
	// json j05{ nullptr };
	// Construct a json value with status "null" and value `nulljson`.
	json j06{ null };
	// Since initializer_list returns a reference to a const object, this method is inefficient.
	// json j07{ json::array_type{ json{0}, json{1} } };
	// json j08{ json::object_type{ { "key0"s, json{ 0 } }, { "key1"s, json{ 1 } } } };
	// Use the helper class templates json::array and json::object for easy and efficient construction of json arrays and json objects.
	// Constructs a json value with the state "array", containing two ordered values 0 and 1.
	json j07{ json::array{ 0, 1 } };
	// Construct a json value with state "object" such that `s08["key0"]==0` and `s08["key1"]==1` is true.
	json j08{ json::object{ "key0"s, 0, "key1"s, 1 } };
	// Copy a json object copies its stored state and values.
	auto j09{ j08 };

	// slice is an accessor and modifier of json values, and the default constructed slice is not associated with any json.
	slice s01{};
	// Use empty() to test if slice is associated with a json.
	auto is_empty{ s01.empty() };
	assert(is_empty); // does nothing
	slice s07{ j07 }; // s07 is associated to j07
	auto is_array{ s07.array() };
	assert(is_array); // does nothing
	// Convert a json value (s01) with state "undefined" to "integer" and set the value to `1`.
	s01 = 1;
	slice s02{ j02 }; // s02 is associated to j02
	// Change the value of j02 to 2.f
	s02 = 2.f;
	// Sets the state of j02 to "undefined" and destroys the previous value.
	s02.reset();
	long long iv{ s07[0] };

	// Iterate j07
	for (auto i : s07.as_array())
	{
		assert(i.integer()); // does nothing
	}
	// Append a value to j07
	// Due to CWG1996, list initialization cannot be used here.
	json::array_type& arr( s07 );
	arr.push_back(json{ 2 });
	slice s08{ j08 };
	// Iterate j08
	for (auto const& [k, v] : s08.as_object())
	{
		assert(v.integer()); // does nothing
	}
	// Insert k-v pairs into j08
	s08["key3"] = 2;
	// Copying a slice is trivial
	auto s08_1{ s08 };

	// const_slice is similar to slice, but has only observers and never modifies the associated json.
	const_slice c01{ s01 };
	// Unlike slice, if the key does not exist in the object then a json_error exception is thrown.
	try
	{
		c01["keyn"];
	}
	catch (bizwen::json_error const& je)
	{
		assert(je.code() == bizwen::json_errc::key_not_found); // does nothing
	}
}