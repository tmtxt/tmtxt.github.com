function* checkExistence(userId) {
	return yield r.table('users').getAll(userId).count().eq(1);
}