var data = [];
  var options = {
    Jenks: 'all',
    team: 'all',
  };
  var margin = { top: 15, right: 15, bottom: 40, left: 80 };
  var width = 515 - margin.right - margin.left;
  var height = 400 - margin.top - margin.bottom;
  var barMax = 8;
  
 // FETCH DATA
  d3.json('datatest.json', function (error, json) {

    if (error) { throw error; }    
    
    data = json;

data.forEach(function(d) {
    d.Jenks = +d.Jenks});

      txdata=data.slice();
    




      QBData = txdata.filter(function (d) {
        return d.POS === 'QB';});
      RBData = txdata.filter(function (d) {
        return d.POS === 'RB';});
      WRData = txdata.filter(function (d) {
        return d.POS === 'WR';});
      TEData = txdata.filter(function (d) {
        return d.POS === 'TE';});

      console.log(TEData);


    if (options.team !== 'all') {
      var team = options.team;

      gxdata = gxdata.filter(function (d) {
        return d.Team === team;
      });
    }


    var vorps = [
       new Vorp('#chart1vorp',QBData, 'POSRANK', 'Position Rank', 'VoRP','QB Expert Consensus VoRP', '.f', ',0s',-40,95,0,20),
       new Vorp('#chart2vorp',RBData, 'POSRANK', 'Position Rank', 'VoRP', 'RB Expert Consensus VoRP', '.0s', ',0s',-80,180,0,45),
       new Vorp('#chart3vorp',WRData, 'POSRANK', 'Position Rank', 'VoRP', 'WR Expert Consensus VoRP', '.0s', ',0s',-60,110,0,45),
       new Vorp('#chart4vorp',TEData, 'POSRANK', 'Position Rank', 'VoRP', 'TE Expert Consensus VoRP', '.0s', ',0s',-30,90,0,20)
     ];


    // EVENT HANDLERS
    d3.select('#Jenks').on('change', function () {
      options.Jenks = d3.event.target.value;
      options.team = 'all';
      QBData = QBData.filter(function (d) {
        return d.Jenks === Jenks;});
      RBData = RBData.filter(function (d) {
        return d.Jenks === Jenks;});
      WRData = WRData.filter(function (d) {
        return d.Jenks === Jenks;});
      TEData = TEData.filter(function (d) {
        return d.Jenks === Jenks;});

      console.log(QBData);
      charts.forEach(function (chart) { chart.update(txdata);});
    });

    
 });
  

  function Vorp(selector, txdata, xvariable, xtitle, yvariable, ytitle, xAxisFormat, yAxisFormat, scaleMin, scaleMax, xmin,xmax) {    
    var vorp = this;

    vorp.xvariable = xvariable;
    vorp.yvariable = yvariable;


    // SVG
    vorp.svg = d3.select(selector)
      .append('svg')
      .attr('width', width + margin.right + margin.left)
      .attr('height', height + margin.top + margin.bottom)
      .append('g')
      .attr('transform', 'translate(' + margin.left + ',' + margin.top + ')');
  
    // SCALES 
    vorp.x = d3.scale.linear()
      .domain([xmin, xmax])
      .range([0, width])
      .nice();

    vorp.y = d3.scale.linear()
      .domain([scaleMin, scaleMax])
      .range([height, 0])
      .nice();

    rscale = d3.scale.sqrt()
      .domain([1, 65])
      .range([1,10]);


     

    // AXES
    var padding=0
    var axispadding=50
    var xaxispadding=35


    // Axes
    var xAxis = d3.svg.axis()
      .tickSize(-height,0)
      .scale(vorp.x)
      .orient("bottom")
      .ticks(10, xAxisFormat);

    var yAxis = d3.svg.axis()
      .orient("left")
      .tickSize(-width)
      .scale(vorp.y)
      .ticks(10, yAxisFormat);
    
    var gx = vorp.svg.append("g")
      .attr("class","y axis")
      .attr("transform","translate(0,"+ height + ")")
      .call(xAxis)
      ;

    var gy = vorp.svg.append("g")
      .attr("class","y axis")
      .attr("transform","translate("+ padding + ",0)")
      .call(yAxis)
      ;

    vorp.svg.append("text")
      .attr("text-anchor", "middle")  // this makes it easy to centre the text as the transform is applied to the anchor
      .attr("class","axislabel")
      .attr("transform", "translate("+ (-axispadding) +","+(height/2)+")rotate(-90)")  // text is drawn off the screen top left, move down and out and rotate
      .text(ytitle);

   vorp.svg.append("text")
      .attr("text-anchor", "middle")  // this makes it easy to centre the text as the transform is applied to the anchor
      .attr("class","axislabel")
      .attr("transform", "translate("+ (width/2) +","+(height+(xaxispadding))+")")
      .text(xtitle);  // centre below axis
      

    var circle = vorp.svg.append("line")
                        .attr('x1', vorp.x(xmin))
                        .attr('y1', vorp.y(0))
                        .attr("x2", vorp.x(xmax))
                        .attr("y2", vorp.y(0))
                        .attr("stroke-width", 2)
                        .attr("stroke", "#F03B20");


     vorp.pointTooltip = d3.tip()
       .attr('class', 'tooltip')
       .offset([-6, 0])
       .html(function(d) { return d.Name +'<br>' + d.POS+', ' + d.Team + '<br> '+'Auction Estimate: '+ (d3.format('$.,0f"'))(d.Auction)+ '<br> '+'Ave. Projection: ' +(d3.format('.1f'))(d.EXPPTS)+ '<br> '+'ESPN Projection: '+(d3.format('.1f'))(d.ESPN)+ '<br> '+'ESPN Overvalue: '+ (d3.format('.2f'))(d.ESPNOverUnder)+ '<br> '+'Tier: '+ (d3.format('.f'))(d.Tier)+ '<br> '+'VoRP: '+ (d3.format('.1f'))(d.VoRP); });

     vorp.pointTooltip(vorp.svg);
 
     vorp.update(txdata);
  }




  Vorp.prototype.update = function(txdata) {
   var vorp = this;
  
      //Options
   
    

    // SCATTERPLOT
    var points = vorp.svg.selectAll('.point, .point2, .point3, .point4')
      .data(txdata);

    points.enter().append('circle')
       .attr('r',0)
       .on('mouseover', vorp.pointTooltip.show)
       .on('mouseout', vorp.pointTooltip.hide);


    points
    .transition().duration(3000).ease('elastic').delay(function(d, i) {
        return i / txdata.length * 800;  // Dynamic delay (i.e. each item delays a little longer)
        }).style('opacity',.7)
      .attr('class',function (d) {if (d.Jenks===10) {return 'point';}
          else if(d.Jenks===9){return 'point2';}
           else if(d.Jenks===8){return 'point3';}
            else if(d.Jenks===7){return 'point4';}
             else if(d.Jenks===6){return 'point5';}
              else if(d.Jenks===5){return 'point6';}
               else if(d.Jenks===4){return 'point7';}
                else if(d.Jenks===3){return 'point8';}
                 else if(d.Jenks===2){return 'point9';}
                  else if(d.Jenks===1){return 'point10';}
        ;})
      .attr('cx', function (d) { return vorp.x(d[vorp.xvariable]); })
      .attr('cy', function (d) { return vorp.y(d[vorp.yvariable]); })
      .attr('r', function (d) { return rscale(d.Auction*1.5); });

    points
    .on("click", function(){
    // Determine if current line is visible
   
    // Hide or show the elements
    d3.select(this).transition().duration(400).style("r",0);

  
  })
    
      ;
    points.exit().transition().duration(1000).delay(150).style("opacity", 0).remove();

      
    }
    // REGRESSION LINE
