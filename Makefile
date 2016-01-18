browserify : clean
	pulp browserify -O -t app.js
clean :
	rm -rf .pulp-cache output app.js
