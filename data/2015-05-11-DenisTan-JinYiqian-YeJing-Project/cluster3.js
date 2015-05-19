runThree();

function runThree() {
function addAxesAndLegendThree (svg, xAxis, yAxis, margin, chartWidth, chartHeight) {

	var axes = svg.append('g')
		.attr('clip-path', 'url(#axes-clip)');

	axes.append('g')
		.attr('class', 'x axis')
		.attr('transform', 'translate(0,' + chartHeight + ')')
		.call(xAxis);

	axes.append('g')
		.attr('class', 'y axis')
		.call(yAxis)
		.append('text')
			.attr('transform', 'rotate(-90)')
			.attr('y', -36)
			.attr('dy', '.71em')
			.style('text-anchor', 'end')
			.text('Normalized Daily-Average Energy Demand (Cluster 3)');


}

var cityThreeOne = "Amman";
var cityThreeTwo = "Springfield";
var cityThreeThree = "Colorado Springs";
var cityThreeFour = "Chattanooga";
var cityThreeFive = "Kansas City";
var cityThreeSix = "Louisville";
var cityThreeSeven = "New York State Elec & Gas Corp";
var cityThreeEight = "Omaha";
var cityThreeNine = "Tokyo";
var cityThreeTen = "Philadelphia";
var cityThreeEleven = "Detroit";
var cityThreeTwelve = "Indianapolis";


function drawPathsThree (svg, data, x, y) {
	var pathOne = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeOne) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeOne) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeOne) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeOne) { return !isNaN(d.MW); } });

	var pathTwo = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeTwo) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeTwo) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeTwo) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeTwo) { return !isNaN(d.MW); } });
	
	var pathThree = d3.svg.line()
		.interpolate('basis')
		.x(function (d) { if (d.city == cityThreeThree) { return x(d.date); } })
		.defined(function(d) { if (d.city == cityThreeThree) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeThree) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeThree) { return !isNaN(d.MW); } });

	var pathFour = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeFour) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeFour) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeFour) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeFour) { return !isNaN(d.MW); } });
		
	var pathFive = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeFive) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeFive) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeFive) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeFive) { return !isNaN(d.MW); } });

	var pathSix = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeSix) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeSix) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeSix) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeSix) { return !isNaN(d.MW); } });
	
	var pathSeven = d3.svg.line()
		.interpolate('basis')
		.x(function (d) { if (d.city == cityThreeSeven) { return x(d.date); } })
		.defined(function(d) { if (d.city == cityThreeSeven) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeSeven) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeSeven) { return !isNaN(d.MW); } });

	var pathEight = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeEight) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeEight) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeEight) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeEight) { return !isNaN(d.MW); } });
		
	var pathNine = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeNine) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeNine) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeNine) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeNine) { return !isNaN(d.MW); } });

	var pathTen = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeTen) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeTen) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeTen) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeTen) { return !isNaN(d.MW); } });
	
	var pathEleven = d3.svg.line()
		.interpolate('basis')
		.x(function (d) { if (d.city == cityThreeEleven) { return x(d.date); } })
		.defined(function(d) { if (d.city == cityThreeEleven) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeEleven) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeEleven) { return !isNaN(d.MW); } });

	var pathTwelve = d3.svg.line()
		.interpolate('basis')
		.x (function (d) { if (d.city == cityThreeTwelve) { return x(d.date) || 1; } })
		.defined(function(d) { if (d.city == cityThreeTwelve) { return !isNaN(d.date); } })
		.y(function (d) { if (d.city == cityThreeTwelve) { return y(d.MW); } })
		.defined(function(d) { if (d.city == cityThreeTwelve) { return !isNaN(d.MW); } });
		
	svg.datum(data);

	svg.append('path')
		.attr('class', 'One')
		.attr('d', pathOne)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#abe3ce")
		.attr("stroke-width", "2.5");

	svg.append('path')
		.attr('class', 'Two')
		.attr('d', pathTwo)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#f4d984")
		.attr("stroke-width", "2.5");
		
	svg.append('path')
		.attr('class', 'Three')
		.attr('d', pathThree)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#fdb59c")
		.attr("stroke-width", "2.5");
		
	svg.append('path')
		.attr('class', 'Four')
		.attr('d', pathFour)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#d6aaea")
		.attr("stroke-width", "2.5");
	
	svg.append('path')
		.attr('class', 'Five')
		.attr('d', pathFive)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#8ab2ea")
		.attr("stroke-width", "2.5");

	svg.append('path')
		.attr('class', 'Six')
		.attr('d', pathSix)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#fcfcfc")
		.attr("stroke-width", "2.5");
		
	svg.append('path')
		.attr('class', 'Seven')
		.attr('d', pathSeven)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#f47920")
		.attr("stroke-width", "2.5");
		
	svg.append('path')
		.attr('class', 'Eight')
		.attr('d', pathEight)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#45a748")
		.attr("stroke-width", "2.5");
	
	svg.append('path')
		.attr('class', 'Nine')
		.attr('d', pathNine)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#ff9af8")
		.attr("stroke-width", "2.5");

	svg.append('path')
		.attr('class', 'Ten')
		.attr('d', pathTen)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#db0000")
		.attr("stroke-width", "2.5");
		
	svg.append('path')
		.attr('class', 'Eleven')
		.attr('d', pathEleven)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "#6a3232")
		.attr("stroke-width", "2.5");
		
	svg.append('path')
		.attr('class', 'Twelve')
		.attr('d', pathTwelve)
		.attr('clip-path', 'url(#rect-clip)')
		.attr('shape-rendering', "crispEdges")
		.attr("fill", "none")
		.attr("stroke", "grey")
		.attr("stroke-width", "2.5");
	
	   svg.selectAll("dot")	
	           .data(data)			
	       .enter().append("circle")								
	           .attr("r", 5)		
	           .attr("cx", function(d) { return x(d.date); })		 
	           .attr("cy", function(d) { return y(d.MW); })
	   	   	.style("fill", "transparent")
	   	.call(d3.helper.tooltip(
	   	       function(d, i){
	   	         //return "<b>Energy Demand</b>"
	   			 return "<b>City: "+d.city+"</b><br>Norm. Demand: "+Math.round(d.MW*100)/100;
	   	       }
	   	   ));

}

function startTransitions (svg, chartWidth, chartHeight, rectClip, x) {
	rectClip
		.transition().duration(0).attr('width', chartWidth);
}

function makeChart (data) {
	var svgWidth  = 840,
		svgHeight = 300,
		margin = { top: 20, right: 20, bottom: 40, left: 60 },
		chartWidth  = svgWidth  - margin.left - margin.right,
		chartHeight = svgHeight - margin.top  - margin.bottom;

	var x = d3.time.scale().range([0, chartWidth])
			.domain(d3.extent(data, function (d) { return d.date; })),
		y = d3.scale.linear().range([chartHeight, 0])
			.domain([-d3.max(data, function (d) {return -d.MW;}), d3.max(data, function (d) { return d.MW; })]);

	var xAxis = d3.svg.axis().scale(x).orient('bottom')
				.innerTickSize(-chartHeight).outerTickSize(0).tickPadding(10),
		yAxis = d3.svg.axis().scale(y).orient('left')
				.innerTickSize(-chartWidth).outerTickSize(0).tickPadding(10);

	var svg = d3.select("#ClusterViz3").append('svg')
		.attr('width',  svgWidth)
		.attr('height', svgHeight)
		.append('g')
			.attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');

	// clipping to start chart hidden and slide it in later
	var rectClip = svg.append('clipPath')
		.attr('id', 'rect-clip')
		.append('rect')
			.attr('width', 0)
			.attr('height', chartHeight);

	addAxesAndLegendThree(svg, xAxis, yAxis, margin, chartWidth, chartHeight);
	drawPathsThree(svg, data, x, y);
	startTransitions(svg, chartWidth, chartHeight, rectClip, x);
	
}

var parseDate  = d3.time.format('%Y-%m-%d').parse;
d3.csv('data/data_cluster3.csv', function (rawData) {

	var data = rawData.map(function (d) {
		return {
			MW: d.MW,
			city: d.city,
			date:  parseDate(d.date),
		};
	});

makeChart(data);
});

};

function removeAll() {
	d3.select('body').selectAll("g")
		.remove();

	d3.select('body').selectAll("path")
		.remove();
		
	d3.select('body').selectAll("clipPath")
		.remove();
		
	d3.select('body').selectAll("polygon")
		.remove();
	
}