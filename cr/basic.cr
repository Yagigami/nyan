cap func(): int32
{
	this_name_is_ok_ :int32 = 6;
	y :int32 = this_name_is_ok_;
	return y;
}

entry func(): int32
{
	first :int32 = 3;
	second :int32 = cap();
	sum :int32 = (first) + second;
	return sum;
}

