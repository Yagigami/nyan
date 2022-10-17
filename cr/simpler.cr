other func() : int32
{
	return 2-1-8+56;
}

entry func() : int32
{
	a :int32 = 1;
	b :int32 = 3;
	return a + b - other();
}

