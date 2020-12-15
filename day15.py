
tests = [
    dict(num=[1, 3, 2], res=1),
    dict(num=[2, 1, 3], res=10),
    dict(num=[1, 2, 3], res=27),
]


def choose(prev, turn, old, older):
    if prev in older:
        return old[prev] - older[prev]
    elif prev in old:
        return 0
    else:
        return 0


def speak(num, turn, old, older):
    if num in old:
        older[num] = old[num]
    old[num] = turn


def run(inpt):
    turn = 1
    a = dict()
    b = dict()
    last = 0

    for i in inpt:
        a[i] = turn
        last = i
        turn += 1

    while turn < 30000001:
        n = choose(last, turn, a, b)
        # print(turn, last, n)
        speak(n, turn, a, b)
        last = n
        turn += 1
    return last


# run([0,3,6])
# for t in tests:
#     print(t['res'], run(t['num']))
print(run([9, 19, 1, 6, 0, 5, 4]))
