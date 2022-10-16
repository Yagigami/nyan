cap func(): int32
{
	this_name_is_ok_ :int32 = 6;
	y :int32 = this_name_is_ok_;
	this_name_is_ok_ = 9;
	return y;
}

uses_bool func(): int32
{
	// a : bool = true;
	b : int = 8 + 0;
	// c : bool = b == cap();
	// d : bool = !(9 < b);
	if (b == 9) {
		return 1;
	}
	else {
		return 2;
	}

	if (1 == 1) {
		if (3 == 4) {
			return 5;
		}
		else {
			return 9;
		}
	}
	else {
		return 4;
	}
	return b;
}

main func(): int32
{
	a : int32 = 1;
	a = 2;
	a = 3;
	return a;
}

entry func(): int32
{
	first :int32 = 3;
	second :int32 = cap();
	sum :int32 = (first) + second - 5;
	return sum;
}

foo func(): int32
{
	r : int32 = 5;
	if (1 == 0) {
		r = 1;
	}
	else {
		if (2 == 3) {
			r = 2;
		}
		else {
			r = 3;
		}
	}
	q : int32 = r;
	return q;
}

bar func() : int32
{
	res : int32 = 4;
	if (main() == cap()) {
		res = 1 - cap();
	}
	return res;
}

