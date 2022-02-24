
//Set initial values
var margin = options.margin,
    width = width-(2*margin),
    height = height-(2*margin),
    y0max = d3.max(data, function(d) { return d.TL; });
    y1max = d3.max(data, function(d) { return d.ACWR; });

// Range of x-axis values
var x = d3.scaleBand()
    .domain(data.map(d => d.day))
    .range([margin, margin+width])
    .paddingInner(0.3)
    .paddingOuter(0.3);

    svg.append("g")
      .attr("transform", "translate(" + 0 + "," + (height+margin) + ")")
      .call(d3.axisBottom(x));

    svg.append("text")
      .attr("transform", "translate(" + (width/2 + 35) + " ," + (height+1.8*margin) + ")")
      .attr("dx", "1em")
      .style("text-anchor", "middle")
      .style("font-family", "Tahoma, Geneva, sans-serif")
      .style("font-size", "12pt")
      .text(options.xLabel);

//Create the y0 axis
   var y0 = d3.scaleLinear()
    .range([height, 0])
    .domain([0, y0max + 10]);
    //.domain([0, 60]);
    //.orient("left")
svg.append("g")
    .attr("transform", "translate(" + margin + ", " + margin + ")")
    .call(d3.axisLeft(y0)
    .ticks(7));
svg.append("text")
    .attr("transform", "translate(" + 0 + " ," + ((height+2*margin)/2) + ") rotate(-90)")
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "Tahoma, Geneva, sans-serif")
    .style("font-size", "12pt")
    .text(options.y0Label);

//Create the y1 axis
   var y1 = d3.scaleLinear()
    .range([height, 0])
    .domain([0, y1max + 0.1]);
    //.domain([0, 1.6]);
    //.orient("right");
svg.append("g")
    .attr("transform", "translate(" + (width+margin) + ", " + margin + ")")
    .call(d3.axisRight(y1)
    .ticks(5));
svg.append("text")
    .attr("transform", "translate(" + (width+2*margin) + " ," + ((height+2*margin)/2) + ") rotate(90)")
    .attr("dy", "1em")
    .style("text-anchor", "middle")
    .style("font-family", "Tahoma, Geneva, sans-serif")
    .style("font-size", "12pt")
    .text(options.y1Label);

//Create the chart title
svg.append("text")
    .attr("x", (width / 2 + 20))
    .attr("y", (margin/2))
    .attr("text-anchor", "middle")
    .attr("dx", "1em")
    .style("font-size", "16pt")
    .style("font-family", "Tahoma, Geneva, sans-serif")
    .text(options.plotTitle);

//Create the chart
svg.selectAll('rect')
    .data(data)
    .enter()
    .append('rect')
    .attr('y', d => (y0(d.TL) + 50))
    .attr('x', (d) => x(d.day))
    .attr('width', x.bandwidth)
    .attr('height', (d) => (height) - y0(d.TL))
    .attr('fill', options.colour)
    .attr('stroke', "#0000FF")
    .attr('stroke-width', "3");

// Add line
var line = d3.line()
        .x(function(d) { return x(d.day); })
        .y(function(d) { return y1(d.ACWR); })
       .curve(d3.curveMonotoneX);

    svg.append("path")
        .datum(data)
        .attr("class", "line")
        .attr("transform", "translate(" + 5 + "," + margin + ")")
        .attr("d", line)
        .style("fill", "none")
        .style("stroke", "black")
        .style("stroke-width", "4");

// Add legend
svg.append("circle")
  .attr("cx",margin + 20)
  .attr("cy",(margin/2 - 10))
  .attr("r", 6)
  .style("fill", "#black");

svg.append("circle")
  .attr("cx", margin + 20)
  .attr("cy",(margin/2 + 10))
  .attr("r", 6)
  .style("fill", "#87CEEB");

svg.append("text")
  .attr("x", margin + 40)
  .attr("y", (margin/2 - 10))
  .text("ACWR")
  .style("font-size", "15px")
  .attr("alignment-baseline","middle");

svg.append("text")
  .attr("x", margin + 40)
  .attr("y", (margin/2 + 10))
  .text("daily loads")
  .style("font-size", "15px")
  .attr("alignment-baseline","middle");


