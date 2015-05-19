map()

function map() {
	var width = 840,
    height = 500;

var projection = d3.geo.mercator()
    .center([0, 5 ])
    .scale(150)
    .rotate([-180,0]);

var svg = d3.select("#Map").append("svg")
    .attr("width", width)
    .attr("height", height);

var path = d3.geo.path()
    .projection(projection);

var g = svg.append("g");

// create color scale
var colscale = d3.scale.ordinal()
	.domain([1, 4])
	.range(["#abe3ce", "#f4d984", "#fdb59c", "#d6aaea"]);

// load and display the World
d3.json("world-110m2.json", function(error, topology) {	

// load and display the cities
d3.csv("cities2.csv", function(error, data) {
    g.selectAll("circle")
       .data(data)
       .enter()
       .append("a")
				  .attr("xlink:href", function(d) {
					  return "https://www.google.com/search?q="+d.city;}
				  )
       .append("circle")
       .attr("cx", function(d) {
               return projection([d.LON, d.LAT])[0];
       })
       .attr("cy", function(d) {
               return projection([d.LON, d.LAT])[1];
       })
       .attr("r", 8)
	   .style("fill", function(d) { return colscale(d.cluster) })
	   .call(d3.helper.tooltip(
	           function(d, i){
	             return "<b>Cluster: "+d.cluster + "</b><br/>City: "+d.city;
	           }
	           ));
});


g.selectAll("path")
      .data(topojson.object(topology, topology.objects.countries)
          .geometries)
    .enter()
      .append("path")
      .attr("d", path)
	  .style("stroke", "black")
	  .style("stroke-width", "0.25px")
	  .style("fill", "#ececec")
});

// zoom and pan
var zoom = d3.behavior.zoom()
    .on("zoom",function() {
        g.attr("transform","translate("+ 
            d3.event.translate.join(",")+")scale("+d3.event.scale+")");
        g.selectAll("circle")
            .attr("d", path.projection(projection));
        g.selectAll("path")  
            .attr("d", path.projection(projection)); 

  });

  //svg.call(zoom);
}