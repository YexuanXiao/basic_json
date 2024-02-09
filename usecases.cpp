#include "basic_json.hpp"
#include <string_view>
int main()
{
	/*
	    Note: This code is for demonstration purposes only and can only be compiled, not run.
	*/
	using j = bizwen::json;
	j j01{};
	j j02{ j01 };
	// j j03{nullptr};  deleted
	j j04{ 1. };
	j j05{ true };
	j j06{ 1.f };
	j j07{ 1u };
	j j08{ 1l };
	j j09{ 1ll };
	j j10{ 1ull };
	j j11{ "aaa" };
	auto str = "bbb";
	j j12{ str, str + 3 };
	j j13{ str, 3 };
	j j14{ str };
	j j15{ std::string_view{ str } };
	j j16{ std::vector<bizwen::json_node<>>{} };
	j j17{ std::map<std::string, bizwen::json_node<>>{} };
	j j18{ bizwen::json_node<>{} };
	swap(j17, j18);
	j17.swap(j18);
	j17 = j18;
	std::swap(j17, j18);
	bizwen::json_node n = std::move(j18).move_to_node();
	using cjs = bizwen::const_json_slice;
	cjs c1;
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
	c1[std::string{ "" }];
	// c1[std::string_view{""}]; requires transparent compare
	c1[""];
	c1[1];
	cjs c2{ j17 };
	c2.swap(c1);
	std::swap(c1, c2);
	cjs c3{ n };
	cjs c4 = c3;
	cjs c5 = std::move(c4);
	c4 = c5;
	c5 = std::move(c4);
	bool b{ c5 };
	bizwen::nulljson_t nj{ c5 };
	std::string const& s{ c5 };
	std::vector<bizwen::json_node<>> const& a{ c5 };
	for (auto const& i : a)
	{
		cjs item{ i };
	}
	std::map<std::string, bizwen::json_node<>> const& o{ c5 };
	for (auto const& [k, v] : o)
	{
		cjs item{ v };
	}
	long long ll{ c5 };
	unsigned long long ull{ c5 };
	using js = bizwen::json_slice;
	js s1{};
	js s2{ j17 };
	cjs c6 = s2;
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