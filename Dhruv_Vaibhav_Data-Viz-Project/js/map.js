var attributeArray = [],
	currentAttribute = 1;

var path = d3.geo.path();

var width,
	height,
	projection,
	d_map,
	playing = false;

function init() {
	setMap();
	animateMap();

	var colorScale = d3.scale.threshold()
	.domain([25.4449,51.8048,143.5311,294.8823,595.5496,740.4915,869.4730,1020.5702,1238.8462,2310.705])
	.range(["#E7C737","#E2B634","#DCA532","#D59531","#CB8631","#C17731", "#B56931", "#A95C31", "#9B5030", "#8D452F"]);
	
horizontalLegend = d3.svg.legend().units("MW").cellWidth(70).cellHeight(25).inputScale(colorScale).cellStepping(100);
d3.select("svg").append("g").attr("transform", "translate(50,550)").attr("class", "legend").call(horizontalLegend);
}

function setMap() {

	var margin = {
			top: 300,
			right: 50,
			bottom: 100,
			left: 50
		},
		width = 900,
		height = 600;

	d_map = d3.select("#map").append("svg")
		.attr("class", "main")
		.attr("width", width)
		.attr("height", height);
	var path = d3.geo.path();

	loadData();
}

function loadData() {
	queue()
		.defer(d3.json, "js/delhiV2.json")
		.defer(d3.csv, "data/DelhiV2.csv")
		.await(processData);
}

function processData(error, delhi, Test) {
	var D_Map = delhi.objects.delhi_sub.geometries;
	for (var i in D_Map) {
		for (var j in Test) {
			if (D_Map[i].properties.id == Test[j].city) {
				for (var k in Test[j]) {
					if (k != 'name' && k != 'id') {
						if (attributeArray.indexOf(k) == -1) {
							attributeArray.push(k);
							// add new column headings to our array for later
						}
						D_Map[i].properties[k] = Number(Test[j][k])
					}
				}
				console.log(D_Map);
				break;
			}
		}
	}
	d3.select('#clock').html(attributeArray[currentAttribute]);
	console.log(attributeArray);
	drawMap(delhi);
}

function drawMap(delhi) {

	var tip = d3.tip()
		.attr('class', 'd3-tip')
		.offset([-10, 0])
		.html(function(d) {
			return "<strong>Demand:</strong> <span style='color:red'>" + d.properties[attributeArray[currentAttribute]] + "</span>";
		});

	d_map.call(tip);

	var featureCollection = topojson.feature(delhi, delhi.objects.delhi_sub);
	var discoms = topojson.feature(delhi, delhi.objects.delhi_sub).features;

	var bounds = d3.geo.bounds(featureCollection);
	var x = d3.sum(bounds, function(d) {
			return d[0];
		}) / 2,
		y = d3.sum(bounds, function(d) {
			return d[1];
		}) / 2;

	var projection = d3.geo.mercator()
		.center([x, y])
		.scale(15000);

	path.projection(projection);

	d_map.selectAll(".discoms")
		.data(discoms)
		.enter().append("path")
		.attr("class", "discoms")
		.attr("id", function(d) {
			return "code_" + d.properties.id
		}, true)
		.attr("d", path)
		.on('mouseover', tip.show)
		.on('mouseout', tip.hide);
		
	var dataRange = getDataRange(); // get the min/max values from the current year's range of data values
	d3.selectAll('.discoms') // select all the countries
		.attr('fill-opactiy', function(d) {
			return getColor(d.properties[attributeArray[currentAttribute]], dataRange); // give them an opacity value based on their current value
		});
}

function sequenceMap() {

	var dataRange = getDataRange(); // get the min/max values from the current year's range of data values

	d3.selectAll('.discoms')
		.transition() //select all the countries and prepare for a transition to new values
		.duration(200) // give it a smooth time period for the transition
		.attr('fill', function(d) {
			return getColor(d.properties[attributeArray[currentAttribute]]); // the end color value
		})
}

function getColor(valueIn) {

	var color = d3.scale.threshold()
		.domain([0, 25.4449,51.8048,143.5311,294.8823,595.5496,740.4915,869.4730,1020.5702,1238.8462,2310.705])
		.range(["#E9D83D","#E7C737","#E2B634","#DCA532","#D59531","#CB8631","#C17731", "#B56931", "#A95C31", "#9B5030", "#8D452F"]);
	
	
	
	//var color = d3.scale.linear() // create a linear scale
	//.domain([valuesIn[0], valuesIn[1]]) // input uses min and max values
	//.range([.3, 1]); // output for opacity between .3 and 1 %

	return color(valueIn); // return that number to the caller
}

function getDataRange() {
	// function loops through all the data values from the current data attribute
	// and returns the min and max values

	var min = Infinity,
		max = -Infinity;
	d3.selectAll('.discoms')
		.each(function(d, i) {
			var currentValue = d.properties[attributeArray[currentAttribute]];
			if (currentValue <= min && currentValue != -99 && currentValue != 'undefined') {
				min = currentValue;
			}
			if (currentValue >= max && currentValue != -99 && currentValue != 'undefined') {
				max = currentValue;
			}
		});
	return [min, max];
}

function animateMap() {

	var timer; // create timer object
	d3.select('#play')
		.on('click', function() { // when user clicks the play button
			if (playing == false) { // if the map is currently playing
				timer = setInterval(function() { // set a JS interval
					if (currentAttribute < attributeArray.length - 1) {
						currentAttribute += 1; // increment the current attribute counter
					} else {
						currentAttribute = 0; // or reset it to zero
					}
					sequenceMap(); // update the representation of the map 
					d3.select('#clock').html(attributeArray[currentAttribute]); // update the clock
				}, 200);

				d3.select(this).html('stop'); // change the button label to stop
				playing = true; // change the status of the animation
			} else { // else if is currently playing
				clearInterval(timer); // stop the animation by clearing the interval
				d3.select(this).html('play'); // change the button label to play
				playing = false; // change the status again
			}
		});
}



window.onload = init();