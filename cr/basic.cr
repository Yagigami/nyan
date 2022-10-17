cap func(): int32
{
	this_name_is_ok_ :int32 = 6;
	y :int32 = this_name_is_ok_;
	this_name_is_ok_ = 9;
	return y;
}

uses_bool func(): int32
{
	a : bool = true;
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

cannot_alias_phi func() : int32
{
	b : int32 = 1;
	a : int32 = 2;
	if (1 != 2) {
		a = b;
	} else {
		b = 9;
	}
	c : int32 = a;
	return b + c;
}

main func(): int32
{
	a : int32 = 1;
	a = 2;
	b : bool = true;
	c : bool = false;
	a = 3;
	return a;
}

entry func(): int32
{
	first :int32 = 3;
	second :int32 = cap();
	wow_this_name_is_extremely_long :int32 = first;
	sum :int32 = (first) + second - 5;
	return sum - cannot_alias_phi() - 2;
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

simple func(): int32
{
	simple : int32 = 0;
	if (true) {
	        simple = 1;
	} else {
	        simple = 2;
	}
	return simple;
}

