	d_map.append("g")
		.attr("class", "d_map")
		.selectAll("path")
		.data(discoms)
		.enter().append("path")
		.attr("d", path)
		.attr("fill", "white")
		.attr("stroke", "black");
	d_map.append("g")
	.attr("class", "x axis")
	// put in middle of screen
	.attr("transform", "translate(" + Swidth / 2 + "," + Sheight / 2 + ")")
	// inroduce axis
	.call(d3.svg.axis()
		.scale(tempScale)
		.orient("bottom")
		.tickFormat(function(d) {
			return formatDate(d);
		})
		.tickSize(0)
		.tickPadding(12)
		.tickValues([tempScale.domain()[0], tempScale.domain()[1]]))
	.select(".domain")
	.select(function() {
		console.log(this);
		return this.parentNode.appendChild(this.cloneNode(true));
	})
	.attr("class", "halo");