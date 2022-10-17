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

