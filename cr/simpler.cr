ndarr func(): int32
{
	a: int32[3][2] = { { 2, 4, 6 }, { 1, 3, 5 } };
	return a[0][2] + a[1][1];
}

entry func() : int32
{
	a :int32 = 1;
	b :int32 = 3;
	if (foo(true))
		return 1;
	return uses_for(4);
}

uses_arr func(base: int32): int32
{
	arr: int32[4] = { 1, 2, 3, 4 };
	i: int32 = base;
	ins: int32 = 0;
	while (i < 4) {
		arr[ins] = i;
		i = i+1;
		ins = ins+1;
	}
	return arr[3];
}

uses_for func(n: int32): int32
{
	sum: int32 = 0;
	i: int32 = 1;
	while (i <= n) {
		sum = sum + i;
		i = i + 1;
	}
	return sum;
}

foo func(x: bool): bool
{
	return !x;
}

