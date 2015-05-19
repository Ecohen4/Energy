function runCThree() {
	
	var dateFirst = [];
	var dataLast = [];
function addAxesAndLegendCThree (svg, xAxis, yAxis, yAxisTwo, margin, chartWidth, chartHeight, textAxes) {
	
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
			.attr('y', -46)
			.attr('dy', '.71em')
			.style('text-anchor', 'end')
			.text(textAxes);
			
	axes.append('g')
		.attr('class', 'y axis')
		.call(yAxisTwo)
		.append('text')
			.attr('transform', 'rotate(-90)')
			.attr('y', -46)
			.attr('dy', '.71em')
			.style('text-anchor', 'end')
			.text(textAxes);

}

function drawPathsCThree (svg, data, x, y, yTwo, titletext, onoff) {
	
var upperOuterArea = d3.svg.area()
	.interpolate('basis')
	.x (function (d) { return x(d.date) || 1; })
	.defined(function(d) { return !isNaN(d.date); })
	.y0(function (d) { return yTwo(d.Hi95); })
	.defined(function(d) { return !isNaN(d.Hi95); })
	.y1(function (d) { return yTwo(d.Hi80); })
	.defined(function(d) { return !isNaN(d.Hi80); })
	;

var upperInnerArea = d3.svg.area()
	.interpolate('basis')
	.x (function (d) { return x(d.date) || 1; })
	.defined(function(d) { return !isNaN(d.date); })
	.y0(function (d) { return yTwo(d.Hi80); })
	.defined(function(d) { return !isNaN(d.Hi80); })
	.y1(function (d) { return yTwo(d.for); })
	.defined(function(d) { return !isNaN(d.for); });
	
var plotFig = d3.svg.line()
	.interpolate('basis')
	.x (function (d) { return x(d.date) || 1; })
	.defined(function(d) { return !isNaN(d.date); })
	.y(function (d) { return y(d.dat); })
	.defined(function(d) { return !isNaN(d.dat); });

var plotFigHalf = d3.svg.line()
	.interpolate('basis')
	.x (function (d) { return x(d.date) || 1; })
	.defined(function(d) { return !isNaN(d.date); })
	.y(function (d) { return yTwo(d.act); })
	.defined(function(d) { return !isNaN(d.act); });

var plotFigTwo = d3.svg.line()
	.interpolate('basis')
	.x (function (d) { return x(d.date) || 1; })
	.defined(function(d) { return !isNaN(d.date); })
	.y(function (d) { return yTwo(d.for); })
	.defined(function(d) { return !isNaN(d.for); });

var lowerInnerArea = d3.svg.area()
	.interpolate('basis')
	.x (function (d) { return x(d.date) || 1; })
	.defined(function(d) { return !isNaN(d.date); })
	.y0(function (d) { return yTwo(d.for); })
	.defined(function(d) { return !isNaN(d.for); })
	.y1(function (d) { return yTwo(d.Lo80); })
	.defined(function(d) { return !isNaN(d.Lo80); });

var lowerOuterArea = d3.svg.area()
	.interpolate('basis')
	.x (function (d) { return x(d.date) || 1; })
	.defined(function(d) { return !isNaN(d.date); })
	.y0(function (d) { return yTwo(d.Lo80); })
	.defined(function(d) { return !isNaN(d.Lo80); })
	.y1(function (d) { return yTwo(d.Lo95); })
	.defined(function(d) { return !isNaN(d.Lo95); });

	svg.datum(data);
		
	svg.append('path')
		.attr('class', 'area upper outer')
		.attr('d', upperOuterArea)
		.attr('clip-path', 'url(#rect-clip)')
		.attr("fill", "rgba(100, 100, 100, 0.1)")
	.attr("stroke", "rgba(100, 100, 100, 0.1)");


	svg.append('path')
		.attr('class', 'area lower outer')
		.attr('d', lowerOuterArea)
		.attr('clip-path', 'url(#rect-clip)')
		.attr("fill", "rgba(100, 100, 100, 0.1)")
	.attr("stroke", "rgba(100, 100, 100, 0.1)");

	svg.append('path')
		.attr('class', 'area upper inner')
		.attr('d', upperInnerArea)
		.attr('clip-path', 'url(#rect-clip)')
		.attr("fill", "rgba(100, 100, 100, 0.4)")
	.attr("stroke", "rgba(100, 100, 100, 0.4)");

	svg.append('path')
		.attr('class', 'area lower inner')
		.attr('d', lowerInnerArea)
		.attr('clip-path', 'url(#rect-clip)')
		.attr("fill", "rgba(100, 100, 100, 0.4)")
		.attr("stroke", "rgba(100, 100, 100, 0.4)");
		
	svg.append('path')
		.attr('class', 'figure')
		.attr('d', plotFig)
		.attr('clip-path', 'url(#rect-clip)')
		.attr("fill", "none")
		.attr("stroke", "#fdb59c")
		   .attr("stroke-width", "2");
		
	svg.append('path')
		.attr('class', 'figure')
		.attr('d', plotFigHalf)
		.attr('clip-path', 'url(#rect-clip)')
		.attr("fill", "none")
		.attr("stroke", "#fdb59c")
		   .attr("stroke-width", "2");
	
	if (onoff == 1) {
	   svg.selectAll("dot")	
	           .data(data)			
	       .enter().append("circle")								
	           .attr("r", 5)		
	           .attr("cx", function(d) { return x(d.date); })		 
	           .attr("cy", function(d) { return yTwo(d.act); })
	   	   	.style("fill", "transparent")
	   	.call(d3.helper.tooltip(
	   	       function(d, i){
				   return "<b>Energy Demand: <br>"+d.act + " MW </b>";
	   	       }
	   	   ));
		  
   	   svg.selectAll("dot")	
   	           .data(data)			
   	       .enter().append("circle")								
   	           .attr("r", 5)		
   	           .attr("cx", function(d) { return x(d.date); })		 
   	           .attr("cy", function(d) { return yTwo(d.for); })
   	   	   	.style("fill", "transparent")
   	   	.call(d3.helper.tooltip(
   	   	       function(d, i){
				   return "<b>Energy Forecast: <br>"+d.for + " MW </b>";
   	   	       }
   	   	   ));

   	   svg.selectAll("dot")	
   	           .data(data)			
   	       .enter().append("circle")								
   	           .attr("r", 5)		
   	           .attr("cx", function(d) { return x(d.date); })		 
   	           .attr("cy", function(d) { return yTwo(d.Lo80); })
   	   	   	.style("fill", "transparent")
   	   	.call(d3.helper.tooltip(
   	   	       function(d, i){
   				   return "<b>80% Confidence: <br>"+d.Lo80 + " MW </b>";
   	   	       }
   	   	   ));	   
      	  
   	   svg.selectAll("dot")	
   	           .data(data)			
   	       .enter().append("circle")								
   	           .attr("r", 5)		
   	           .attr("cx", function(d) { return x(d.date); })		 
   	           .attr("cy", function(d) { return yTwo(d.Hi80); })
   	   	   	.style("fill", "transparent")
   	   	.call(d3.helper.tooltip(
   	   	       function(d, i){
   				   return "<b>80% Confidence: <br>"+d.Hi80 + " MW </b>";
   	   	       }
   	   	   ));		   

   	   svg.selectAll("dot")	
   	           .data(data)			
   	       .enter().append("circle")								
   	           .attr("r", 5)		
   	           .attr("cx", function(d) { return x(d.date); })		 
   	           .attr("cy", function(d) { return yTwo(d.Lo95); })
   	   	   	.style("fill", "transparent")
   	   	.call(d3.helper.tooltip(
   	   	       function(d, i){
   				   return "<b>95% Confidence: <br>"+d.Lo95 + " MW </b>";
   	   	       }
   	   	   ));		   

   	   svg.selectAll("dot")	
   	           .data(data)			
   	       .enter().append("circle")								
   	           .attr("r", 5)		
   	           .attr("cx", function(d) { return x(d.date); })		 
   	           .attr("cy", function(d) { return yTwo(d.Hi95); })
   	   	   	.style("fill", "transparent")
   	   	.call(d3.helper.tooltip(
   	   	       function(d, i){
   				   return "<b>95% Confidence: <br>"+d.Hi95 + " MW </b>";
   	   	       }
   	   	   ));
		   
	   };
	   
   svg.selectAll("dot")	
           .data(data)			
       .enter().append("circle")								
           .attr("r", 5)		
           .attr("cx", function(d) { return x(d.date); })		 
           .attr("cy", function(d) { return y(d.dat); })
   	   	.style("fill", "transparent")
   	.call(d3.helper.tooltip(
   	       function(d, i){
			   return "<b>Component value: <br>"+d.dat + "</b>";
   	       }
   	   ));
		
	svg.append('path')
		.attr('class', 'figure')
		.attr('d', plotFigTwo)
		.attr('clip-path', 'url(#rect-clip)')
		.attr("fill", "none")
		.attr("stroke", "#000000")
	   .attr("stroke-width", "2");
		
	svg.append('text')
		.attr('x', 620)
		.attr('y', 30)
		.attr('font-family', 'Open Sans')
		.attr('font-size', '14px')
		.text(titletext)

}

function startTransitions (svg, chartWidth, chartHeight, rectClip, x) {
	rectClip
		.transition().duration(30000).attr('width', chartWidth);

}

function makeChartCThree (data, titletext, textAxes, onoff) {
	var svgWidth  = 840,
		svgHeight = 300,
		margin = { top: 20, right: 20, bottom: 40, left: 60 },
		chartWidth  = svgWidth  - margin.left - margin.right,
		chartHeight = svgHeight - margin.top  - margin.bottom;

	var x = d3.time.scale().range([0, chartWidth])
			.domain([dateFirst, dateLast]),
		y = d3.scale.linear().range([chartHeight, 0])
			.domain(d3.extent(data, function (d) { return d.dat; })),
		yTwo = d3.scale.linear().range([chartHeight, 0])
			.domain([d3.extent(data, function (d) { return d.Lo95; })[0],d3.extent(data, function (d) { return d.Hi95; })[1]]);

	var xAxis = d3.svg.axis().scale(x).orient('bottom')
				.innerTickSize(-chartHeight).outerTickSize(0).tickPadding(10),
		yAxis = d3.svg.axis().scale(y).orient('left')
				.innerTickSize(-chartWidth).outerTickSize(0).tickPadding(10),
		yAxisTwo = d3.svg.axis().scale(yTwo).orient('left')
			.innerTickSize(-chartWidth).outerTickSize(0).tickPadding(10);

	var svg = d3.select('#City3').append('svg')
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

	addAxesAndLegendCThree(svg, xAxis, yAxis, yAxisTwo, margin, chartWidth, chartHeight, textAxes);
	drawPathsCThree(svg, data, x, y, yTwo, titletext, onoff);
	startTransitions(svg, chartWidth, chartHeight, rectClip, x);
}



var parseDate  = d3.time.format('%Y-%m-%d').parse;

d3.csv('data/Louisville-predict-365.csv', function (rawData) {

	var dataSix = rawData.map(function (d) {
		return {
			date: d3.time.format('%d/%m/%y').parse(d.Date),
			for: Math.round(d.Forecast),
			act: Math.round(d.Actual),
			Lo80: Math.round(d.Lo80),
			Lo95: Math.round(d.Lo95),
			Hi80: Math.round(d.Hi80),
			Hi95: Math.round(d.Hi95),
		};
	});
	
	dateLast = d3.extent(dataSix, function(d) {
		return d.date;
	})[1];

	d3.csv('data/Louisville-original.csv', function (rawData) {

		var dataOne = rawData.map(function (d) {
			return {
				date: parseDate(d.day),
				dat: Math.round(d.MW),
			};
		});
		var dataTwo = rawData.map(function (d) {
			return {
				date: parseDate(d.day),
				dat: Math.round(d.Temp),
			};
		});
		var dataThree = rawData.map(function (d) {
			return {
				date: parseDate(d.day),
				dat: Math.round(d.Trend),
			};
		});
		var dataFour = rawData.map(function (d) {
			return {
				date: parseDate(d.day),
				dat: Math.round(d.Random),
			};
		});
		var dataFive = rawData.map(function (d) {
			return {
				date: parseDate(d.day),
				dat: Math.round(d.Seasonal),
			};
		});

		dateFirst = d3.extent(dataOne, function (d) {
			return d.date;
		})[0]

	var titletextOne = 'Energy Demand',
		titletextTwo = 'Demand & Prediction',
		titletextThree = 'Temperature',
		titletextFour = 'Trend',
		titletextFive = 'Random',
		titletextSix = 'Seasonal',
		textAxesOne = 'Louisville Daily Average Energy Demand (MW)',
		textAxesTwo = 'Louisville Daily Average Energy 365-Day Forecast (MW)',
		textAxesThree = 'Louisville Daily Average Temperature (C)',
		textAxesFour = 'Trend Component',
		textAxesFive = 'Random Component',
		textAxesSix = 'Seasonal Component';
	

//makeChartCThree(dataOne, titletextOne, textAxesOne);
makeChartCThree(dataSix, titletextTwo, textAxesTwo, 1);
//makeChartCThree(dataTwo, titletextThree, textAxesThree);
makeChartCThree(dataThree, titletextFour, textAxesFour, 0);
makeChartCThree(dataFour, titletextFive, textAxesFive, 0);
makeChartCThree(dataFive, titletextSix, textAxesSix, 0);
});
});
};