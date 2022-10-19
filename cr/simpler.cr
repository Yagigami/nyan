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

add func(a: int32, b: int32): int32
{
	return a + b;
}

other func() : int32
{
	a :int32 = 8;
	if (true)
		if (true)
			a = 0;
		else
			a = 4;
	else
		if (false)
			a = 3;
		else
			a = 0;
	return a;
}

entry func() : int32
{
	a :int32 = 1;
	b :int32 = 3;
	return a + b - other();
}

