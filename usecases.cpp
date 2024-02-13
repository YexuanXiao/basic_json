#include "basic_json.hpp"
#include <string_view>
int main()
{
	/*
	    Note: This code is for demonstration purposes only and can only be compiled, not run.
	*/
	// json
	using json = bizwen::json;
	using namespace std::literals;
	json j01{};
	json j02{ j01 };
	// j j03{ nullptr };  deleted
	json j04{ 1. };
	json j05{ true };
	json j06{ 1.f };
	json j07{ 1u };
	json j08{ 1l };
	json j09{ 1ll };
	json j10{ 1ull };
	json j11{ "aaa" };
	auto str = "bbb";
	json j12{ str, str + 3 };
	json j13{ str, 3 };
	json j14{ str };
	json j15{ "bbb"sv };
	// since initializer_list returns a reference to a const object, this method is inefficient
	// json j16{ json::array_type{ json{0}, json{1} } };
	// json j17{ json::object_type{ { "key0"s, json{ 0 } }, { "key1"s, json{ 1 } } } };
	json j16{ json::array{ 0, 1 } };
	json j17{ json::object{"key0"s, 0, "key1"s, 1} };
	json j18{ bizwen::basic_json_node<>{} };
	swap(j17, j18); // adl hidden friend
	j17.swap(j18);
	j17 = j18;
	std::swap(j17, j18);
	json::node_type n{ std::move(j18) };

	// const_slice
	using const_slice = bizwen::const_json_slice;
	const_slice c1;
	c1.empty();
	c1.array();
	c1.string();
	c1.null();
	c1.boolean();
	c1.number();
	c1.object();
	c1.array();
	c1.object();
	c1.integer();
	c1.uinteger();
	c1["key"];
	c1["key"s];
	// requires transparent comparable
	// c1["key"sv];
	c1[1];
	const_slice c2{ j17 };
	c2.swap(c1);
	std::swap(c1, c2);
	const_slice c3{ std::move(j17) };
	const_slice c4 = c3;
	const_slice c5 = std::move(c4);
	c4 = c5;
	c5 = std::move(c4);
	bool b{ c5 };
	bizwen::nulljson_t nj{ c5 };
	json::string_type const& s{ c5 };
	json::array_type const& a{ c5 };
	for (auto const& i : a)
	{
		const_slice item{ i };
	}
	json::object_type const& o{ c5 };
	for (auto const& [k, v] : o)
	{
		const_slice item{ v };
	}
	long long ll{ c5 };
	unsigned long long ull{ c5 };

	// slice
	using slice = bizwen::json_slice;
	slice s1{};
	slice s2{ j17 };
	const_slice c6 = s2;
	std::string str1;
	s2 = str1;
	s2 = std::string{};
	s2 = "aaa";
	s2 = std::string_view{};
	s2 = bizwen::nulljson;
	s2 = true;
	s2 = 1.;
	s2 = 1;
	s2 = 1u;
	s2 = 1ll;
	s2 = 1ull;
	s2 = n;
}
