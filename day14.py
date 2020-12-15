import functools
import itertools

test = """mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0"""


def part1(data):
    def parse(packet):
        def content(line, mp, mn):
            idx = line.split('[')[1].split(']')[0]
            v = int(line.split('= ')[1])
            v = (v | mp) & (~mn)
            return (idx, v)

        lines = packet.strip().split('\n')
        mp = functools.reduce(lambda x, acc: x | acc, map(lambda x: 2 ** int(x[0]), filter(
            lambda x: x[1] == '1', enumerate(reversed(lines[0].split('= ')[1])))))
        mn = functools.reduce(lambda x, acc: x | acc, map(lambda x: 2 ** int(x[0]), filter(
            lambda x: x[1] == '0', enumerate(reversed(lines[0].split('= ')[1])))))
        contents = dict(map(lambda x: content(x, mp, mn), lines[1:]))
        return contents
    insts = (list(map(parse, data.split('mask')[1:])))
    mem = dict()

    for d in insts:
        for (k, v) in d.items():
            mem[k] = v

    return sum(mem.values())


data = open('inputs/14.txt').read()
print('part 1:', part1(data))


def part2(data):
  def parse(packet):
    def content(line, mp, mn):
      idx = int(line.split('[')[1].split(']')[0])
      idx = idx | mp

      v = int(line.split('= ')[1])
      idxs = {idx: v}

      for n in itertools.product(*((0,1) for _ in range(len(mn)))):
        x = idx
        for j,i in zip(mn, n):
          if i == 1:
            x &= ~j
          else:
            x |= j
        idxs[x] = v

      return idxs

    lines = packet.strip().split('\n')
    mp = 0
    mpp = list(map(lambda x: 2 ** int(x[0]), filter(
        lambda x: x[1] == '1', enumerate(reversed(lines[0].split('= ')[1])))))
    if len(mpp) > 0:
      mp = functools.reduce(lambda x, acc: x | acc, mpp)
    mn = list(map(lambda x: 2 ** int(x[0]), filter(
        lambda x: x[1] == 'X', enumerate(reversed(lines[0].split('= ')[1])))))
    contents = list(map(lambda x: content(x, mp, mn), lines[1:]))

    return contents

  insts = (list(map(parse, data.split('mask')[1:])))
  mem = dict()

  for d in insts:
    # print(d)
    for d2 in d:
      for (k, v) in d2.items():
          mem[k] = v

  return sum(mem.values())

print(part2(data))
