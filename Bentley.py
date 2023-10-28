import random, typing, sys, time


def make_sources():
    sources = []
    for count in range(10000):
        workingEx = []
        for u in range(1000): workingEx.append(random.randrange(sys.maxsize))
        sources.append(workingEx)
    return sources


source_list = make_sources()


def bentley_QS(arr: list[int]):
    if len(arr) <= 1: return arr

    def swap(tar1: int, tar2: int):
        buff = arr[tar1]
        arr[tar1],arr[tar2] = arr[tar2], buff

    def qs(start: int, end: int):
        if start >= end: return

        pivot_swap = start
        for focus in range (start+1, end+1):
            if arr[start] > arr[focus]:
                pivot_swap += 1
                swap(pivot_swap, focus)

        swap(start, pivot_swap)

        qs(start, pivot_swap - 1)
        qs(pivot_swap + 1, end)

    qs(0, len(arr)-1)


def check_list_sorted(arr: list[int]):
    for u in range(0,len(arr) - 1):
        if arr[u] > arr[u+1]: return False
    return True


def check_list(arr: list[list[int]]):
    for elem in arr:
        if not check_list_sorted(elem):
            return False
    return True


print("Started QS-ing")


start_time = time.perf_counter()


print(start_time)


for ls in source_list: bentley_QS(ls)


print(time.perf_counter() - start_time)


print(check_list(source_list))
