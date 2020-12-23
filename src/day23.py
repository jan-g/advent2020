#!/usr/bin/python


top = 1_000_000
its = 10_000_000


def main():
    inp = [9, 1, 6, 4, 3, 8, 2, 7, 5]
    # inp = [3, 8, 9, 1, 2, 5, 4, 6, 7]
    inp += range(max(inp) + 1, top + 1)

    l = [0] * (1 + max(top, len(inp)))

    for idx, v in enumerate(inp):
        vv = inp[(idx + 1) % len(inp)]
         #print(idx, v, vv, (idx + 1) % len(inp))
        l[v] = vv

    head = inp[0]

    for iter in range(its):
        if iter % 1000 == 0:
            print(iter, head)
        a = l[head]
        b = l[a]
        c = l[b]
        d = l[c]

        n = (head - 2) % top + 1
        if n == a or n == b or n == c:
            n = (n - 2) % top + 1
            if n == a or n == b or n == c:
                n = (n - 2) % top + 1
                if n == a or n == b or n == c:
                    n = (n - 2) % top + 1

        n2 = l[n]

        l[n] = a
        l[c] = n2
        l[head] = d
        head = d

    a = l[1]
    b = l[a]
    print (a, b, a * b)


if __name__ == '__main__':
    main()
