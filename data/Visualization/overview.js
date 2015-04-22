function overview() {
d3.select("body")
	.append("svg")
	.attr("width", 800)
	.attr("height", 500)
	.append("g")
	.append("svg:image")
	.attr("xlink:href", "overview.jpg")
	.attr("width", 800)
	.attr("height", 500)
	.attr("x", 10)
	.attr("y", 10);
}