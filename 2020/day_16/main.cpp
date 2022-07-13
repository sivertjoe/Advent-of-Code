#include <iostream>
#include <map>
#include <sstream>
#include <stdint.h>
#include <stdio.h>
#include <vector>

typedef std::vector<int> Ticket;

struct Range
{
    Range(std::string line);
    bool inside(int val);

    int low;
    int high;
};

bool Range::inside(int val) { return val >= low && val <= high; }

Range::Range(std::string line)
{
    auto spl = line.find("-");
    auto first = line.substr(0, spl);
    auto second = line.substr(spl + 1, line.size() - spl);

    this->low = std::stoi(first);
    this->high = std::stoi(second);
}

struct Rule
{
    Rule(std::string);
    bool inside(int val);
    void print();
    std::vector<Range> range;
};

bool Rule::inside(int val)
{
    return range[0].inside(val) || range[1].inside(val);
}

Rule::Rule(std::string line)
{
    auto colon = line.find(": ");
    auto or_word = line.find(" or ");

    auto first = line.substr(colon + 2, or_word - colon - 2);
    auto second = line.substr(or_word + 4, line.length() - or_word - 4);

    Range range1(first);
    Range range2(second);

    this->range.push_back(range1);
    this->range.push_back(range2);
}

struct Input
{
    std::vector<Rule> rules;
    Ticket my_ticket;
    std::vector<Ticket> other_tickets;
};

auto parse_numbers(std::string line) -> std::vector<int>
{
    std::vector<int> vec;
    std::stringstream ss(line);

    for (int i; ss >> i;)
    {
        vec.push_back(i);
        if (ss.peek() == ',')
            ss.ignore();
    }
    return vec;
}

auto read_input() -> Input
{
    Input input;
    std::vector<std::string> res;
    std::string line;

    auto val = 0;

    while (std::getline(std::cin, line))
    {
        if (line.empty())
        {
            val++;
            continue;
        }

        if (line.find("your ticket") != std::string::npos ||
            line.find("nearby") != std::string::npos)
            continue;

        switch (val)
        {
        case 0:
            input.rules.push_back(Rule(line));
            break;
        case 1:
            input.my_ticket = parse_numbers(line);
            break;
        case 2:
            input.other_tickets.push_back(parse_numbers(line));
            break;
        }
    }
    return input;
}

auto create_range_map(Input& input) -> std::map<int, bool>
{
    std::map<int, bool> map;
    for (auto &rule : input.rules)
    {
        auto &r = rule.range[0];
        assert(r.low < r.high);
        for (int i = r.low; i != r.high; i++)
        {
            map[i] = true;
        }

        auto &r2 = rule.range[1];
        assert(r2.low < r2.high);
        for (int i = r2.low; i != r2.high; i++)
        {
            map[i] = true;
        }
    }
    return map;
}

auto part_one(Input &input) -> int
{
    auto map = create_range_map(input);
    auto sum = 0;

    for (auto &ticket : input.other_tickets)
    {
        for (auto &elem : ticket)
        {
            if (!map[elem])
            {
                sum += elem;
            }
        }
    }

    return sum;
}

auto discard_invalid_tickets(Input &input) -> std::vector<Ticket>
{
    std::vector<Ticket> res;

    auto map = create_range_map(input);


    for (auto &ticket : input.other_tickets)
    {
        bool valid = true;
        for (auto &elem : ticket)
        {
            if (!map[elem])
            {
                valid = false;
                break;
            }
        }
        if (valid)
            res.push_back(ticket);
    }

    return res;
}

auto indexes(std::vector<std::vector<int>> pos) -> std::map<int, int>
{
    std::vector<int> res;
    std::map<int, int> map;
    std::map<int, int> occ;

    for (auto &v : pos)
    {
        for (auto &elem : v)
        {
            occ[elem] += 1;
        }
    }

    auto count = 1;
    auto end = pos.size();
    while (map.size() != end)
    {
    // Goto 😏
    firstt:
        for (int r = 0; r < pos.size(); r++)
        {
            for (auto &elem : pos[r])
            {
                if (occ[elem] == count)
                {
                    pos[r].clear();
                    map[r] = elem;
                    count += 1;
                    goto firstt;
                }
            }
        }
    }

    return map;
}

auto part_two(Input &input) -> uint64_t
{
    auto tickets = discard_invalid_tickets(input);

    int num_tickets = tickets.size();
    int ticket_length = tickets[0].size();

    std::vector<std::vector<int>> pos;

    for (int r = 0; r < input.rules.size(); r++)
    {
        Rule &rule = input.rules[r];
        pos.push_back({});
        for (int j = 0; j < ticket_length; j++)
        {
            bool match = true;
            for (int i = 0; i < num_tickets; i++)
            {
                auto val = tickets[i][j];
                if (!rule.inside(val))
                {
                    match = false;
                    break;
                }
            }
            if (match)
            {
                pos[r].push_back(j);
            }
        }
    }

    auto &v = input.my_ticket;

    auto map = indexes(pos);
    uint64_t sum = 1;
    for (int i = 0; i < 6; i++)
    {
        sum *= v[map[i]];
    }
    return sum;
}

int main()
{
    auto input = read_input();
    std::cout << part_one(input) << std::endl;
    std::cout << part_two(input) << std::endl;
}
