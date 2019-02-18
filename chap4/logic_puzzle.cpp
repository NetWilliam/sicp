/*
 * logic_puzzle.cpp
 * Copyright (C) 2019 liuweibo <liuweibo@liuweibo-ThinkPad-T450>
 *
 * Distributed under terms of the MIT license.
 */

//#include "logic_puzzle.h"
#include <algorithm>
#include <assert.h>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>

using namespace std;

class PermutationGenerator : public std::iterator<std::input_iterator_tag, std::vector<unsigned>>
{
    typedef void (PermutationGenerator::*BoolLike)();
    void non_comparable();

  public:
    using iterator_category = std::input_iterator_tag;
    using value_type = std::vector<unsigned>;
    using reference = value_type const &;
    using pointer = value_type const *;
    using difference_type = ptrdiff_t;

    /*
    typedef std::input_iterator_tag iterator_category;
    typedef std::vector<unsigned> value_type;
    typedef value_type const &reference;
    typedef value_type const *pointer;
    typedef ptrdiff_t difference_type;
    */

    PermutationGenerator(int perm_size)
    {
        permutation.resize(perm_size);
        std::generate(permutation.begin(), permutation.end(), [] {
            static unsigned i = 1;
            return i++;
        });
        done = false;
        init = true;
    }

    explicit operator bool() const { return !done; }
    operator BoolLike() const { return done ? 0 : &PermutationGenerator::non_comparable; }
    reference operator*() const { return permutation; }
    pointer operator->() const { return &permutation; }

    PermutationGenerator &operator++()
    {
        assert(!done);
        if (init)
        {
            init = false;
            return *this;
        }
        if (std::next_permutation(permutation.begin(), permutation.end()))
        {
            return *this;
        }
        done = true;
        return *this;
    }

    PermutationGenerator operator++(int)
    {
        PermutationGenerator const tmp(*this);
        ++*this;
        return tmp;
    }

  private:
    value_type permutation;
    bool init;
    bool done;
};
// 写代码不是应该带来快乐的吗? 为什么会这样子

enum Dweller
{
    baker,
    cooper,
    fletcher,
    miller,
    smith,
    first = baker,
    last = smith
};

void gen_combination() {}

int main()
{
    using D = Dweller;
    auto pg = PermutationGenerator(D::last + 1);
    while (++pg)
    {
        const auto &floors = *pg;
        if (floors[D::baker] != 5 and floors[D::cooper] != 1 and floors[D::fletcher] != 5 and
            floors[D::fletcher] != 1 and floors[D::miller] > floors[D::cooper] and
            (abs(floors[D::smith] - floors[D::fletcher]) != 1) and (abs(floors[D::fletcher] - floors[D::cooper]) != 1))
        {

            cout << "baker: " << floors[D::baker] << " cooper: " << floors[D::cooper]
                 << " fletcher: " << floors[D::fletcher] << " miller: " << floors[D::miller]
                 << " smith: " << floors[D::smith] << endl;
        }
    }
    cout << "expect:\nbaker: 3 cooper: 2 fletcher: 4 miller: 5 smith: 1" << endl;

    if (false)
    {
        auto pg = PermutationGenerator(3);
        while (++pg)
        {
            const auto &arr = *pg;
            cout << arr[0] << " " << arr[1] << " " << arr[2] << endl;
        }
        const auto &arr = *pg;
        cout << "after mutation " << arr[0] << " " << arr[1] << " " << arr[2] << endl;
    }
    return 0;
}
