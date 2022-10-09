decl entry func(): int32
{
	decl first int32 = 3;
	decl second int32 = cap();
	decl sum int32 = (first) + second;
	return sum;
}

decl cap func(): int32
{
	return 6;
}

