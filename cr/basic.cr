cap func(): int32
{
	this_name_is_ok_ :int32 = 6;
	y :int32 = this_name_is_ok_;
	this_name_is_ok_ = 9;
	return y;
}

uses_bool func(): bool
{
	a : bool = true;
	b : int = 8 + 0;
	c : bool = b == cap();
	d : bool = !(9 < b);
	return a;
}

main func(): int32
{
	return 0;
}

entry func(): int32
{
	first :int32 = 3;
	second :int32 = cap();
	sum :int32 = (first) + second - 5;
	return sum;
}

