other func() : int32
{
	a :int32 = 8;
	if (true)
		a = a - 2;
	else
		a = a + 3;
	return a;
}

entry func() : int32
{
	a :int32 = 1;
	b :int32 = 3;
	return a + b - other();
}

