
decl cap func(): int32
{
	decl this_name_is_ok_ int32 = 6;
	decl y int32 = this_name_is_ok_;
	return y;
}

decl entry func(): int32
{
	decl first int32 = 3;
	decl second int32 = cap();
	decl sum int32 = (first) + second;
	return sum;
}

