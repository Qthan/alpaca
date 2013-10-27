let pow base exp = 
	let res = ref 1 in
	let b = ref base in
	let e = ref exp in
		while (!e > 0) do
			if (!e mod 2 = 1) then res := (!res)*(!b);
			e := !e / 2;
			b := (!b)*(!b)
		done;
		(!res)


