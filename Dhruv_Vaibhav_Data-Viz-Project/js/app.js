//data


//Setting map variables
var margin = {
		top: 50,
		right: 50,
		bottom: 50,
		left: 50
	},
	width = 900,
	height = 600 - margin.top - margin.bottom;

var parent = d3.select("div.container-fluid");
var child = parent.select("div.row-fluid");


var d_map = child.select("div.span1").append("svg")
	.attr("class", "main")
	.attr("width", width)
	.attr("height", height);
var path = d3.geo.path();

//map
d3.json("js/delhiV2.json", function(error, delhi) {
	if (error) {
		return console.error(error)
	} else {
		console.log(delhi);
	}
	var featureCollection = topojson.feature(delhi, delhi.objects.delhi_sub);
	var discoms = topojson.feature(delhi, delhi.objects.delhi_sub).features;
	console.log(discoms);
	var bounds = d3.geo.bounds(featureCollection);
	var x = d3.sum(bounds, function(d) {
			return d[0];
		}) / 2,
		y = d3.sum(bounds, function(d) {
			return d[1];
		}) / 2;

	var projection = d3.geo.mercator()
		.center([x, y])
		.scale(18000);

	path.projection(projection);

	d_map.selectAll(".delhi_licensee_final")
		.data(discoms)
		.enter().append("path")
		.attr("class", function(d) {
			return "delhi_licensee_final " + d.id;
			console.log(d);
		})
		.attr("d", path);
		
});









//sliders
formatDate = d3.time.format("%b %d '%y");

var Smargin = {
		top: 200,
		right: 50,
		bottom: 200,
		left: 50,
	},
	Swidth = 960 - Smargin.left - Smargin.right,
	Sheight = 500 - Smargin.bottom - Smargin.top;

var slid = child.select("div.span2").append("svg")
	.attr("class","slid")
	.attr("width", Swidth)
	.attr("height", height);

// scale function
var timeScale = d3.time.scale()
	.domain([new Date('2011-01-01'), new Date('2013-04-01')])
	.range([0, Swidth])
	.clamp(true);

var tempScale = d3.scale.linear()
	.domain([0, 180])
	.range([0, Swidth])
	.clamp(true);


// initial value
var startValue = timeScale(new Date('2011-01-1'));
startingValue = new Date('2012-01-01');

//var brush = d3.svg.brush()
	//.x(timeScale)
	//.extent([startingValue, startingValue])
	//.on("brush", brushed);
var brush = d3.svg.brush()
	.x(tempScale)
	.extent([0,0])
	.clamp(true);

slid.append("g")
	.attr("class", "x axis")
	.attr("transform", "translate(" + Swidth / 2 + "," + Sheight / 2 + ")")
	.call(d3.svg.axis()
		.scale(tempScale)
		.orient("bottom")
		.tickFormat(function(d) {
			return d + "°";
		})
		.tickSize(0)
		.tickPadding(12))
	.select(".domain")
	.select(function() {
		return this.parentNode.appendChild(this.cloneNode(true));
	})
	.attr("class", "halo");

var slider = slid.append("g")
	.attr("class", "slider")
	.call(brush);

slider.selectAll(".extent, .resize")
	.remove();

slider.select(".background")
	.attr("height", Sheight);

var handle = slider.append("g")
	.attr("class", "handle");

handle.append("path")
	.attr("transform", "translate(" + Swidth / 2 + "," + Sheight / 2 + ")")
	.attr("d", "M 0 -20 V 20");
//handle.append('text')
	//.text(startingValue)
	//.attr("transform", "translate(" + Swidth / 2 + " ," + (Sheight / 2 - 25) + ")");
slider
	.call(brush.event)
	.transition() // gratuitous intro!
	.duration(750)
	.call(brush.extent([70, 70]))
	.call(brush.event);

function brushed() {
	var value = brush.extent()[0];

	if (d3.event.sourceEvent) { // not a programmatic event
		value = tempScale.invert(d3.mouse(this)[0]);
		brush.extent([value, value]);
	}

	handle.attr("transform", "translate(" + tempScale(value) + ",0)");
	d3.select("body").style("background-color",d3.hsl(value, .8, .8))
	//d3.select("path.delhi_licensee_final.MES").style("fill",d3.)
	//handle.select('text').text(formatDate(value));
}



