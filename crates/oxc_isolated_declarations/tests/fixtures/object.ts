const A = {
	get state(): number {
		return 0;
	},
};

const B = {
	set state(v: string) {
		// do something
	},
};

const C = {
	get state(): number {
		return 0;
	},
	set state(v: number) {
		// do something
	},
};

const D = {
	get state(): number {
		return 0;
	},
	set state(v: string) {
		// do something
	},
};

const E = {
	get state() {
		return A;
	},
	set state(v) {
		// do something
	},
};

const ObjectMethods = {
	a() {
		return 0;
	},
	b(): number {},
	c() {},
};

const ObjectKeys = {
	a: 0,
	["b"]: 1,
	[`c`]: 2,
	[3]: 3,
	[-3]: 4,
	[4n]: 5,
};
