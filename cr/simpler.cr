mut func(): int32
{
	mut :int32 = 1;
	mut = 2;
	mut = 3;
	return mut + 1;
}

sum func(n: int32): int32
{
	if (n == 0)
		return 0;
	else {
		s: int32 = n-1;
		r: int32 = n + sum(s);
		return     n + sum(s);
	}
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

entry func() : int32
{
	a :int32 = 1;
	b :int32 = 3;
	if (foo(true))
		return 1;
	return uses_for(4);
}

