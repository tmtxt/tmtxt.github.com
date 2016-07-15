---
layout: post
title: "Moving away from D3.js, I'm using React.js for DOM Manipulation now"
description: ""
categories: [javascript]
tags: []
thumbnail: 
---

# Why React in place of D3?

So, I'm migrating my web app to full client-side rendering using **React** recently. The reason is
that I mixed both server-side rendering and client-side render too much. At first, all the pages use
server  rendering (something like jinja2 template). However, as a consequence of the increase in
interaction  with user on the web app, I started to add more and more js code, which leads to the
logic duplication in both backend and frontend. I decided to move all the rendering to React.js and
that makes my life much easier dealing with all the DOM manipulation and two-way binding.

The only
thing I need to deal with is the diagram that I implemented using **D3.js** before. I have been
researching on the internet for a good solution and I was very closed to follow one of those
tutorials, which suggests hooking **D3.js** rendering in **React**'s `componentDidMount` event
(actually, most of the tutorials suggest that). Suddenly, one of my frontend friend recommended me
throwing away **D3.js** for all those tasks. He said that **React.js** is very good at DOM
manipulation stuff, why I have to mixed D3 inside that to lose all the flexibility of two-way
binding, virtual DOM update,... Yeah, that sounds logical and I decided to give it a try, threw away
all my old code and started in a fresh new React way. Of course, I didn't **D3.js** completely, I
still use it because of its supporting functions for calculating the diagram position and
coordination.

# Implement the Tree diagram in React.js

Okay, the first thing I need to do is to convert this old piece of code from **D3** to **React**.
In contrast to my imagination, rendering the tree diagram using **React** is an amazingly
effortless task. Let's compare the old D3 and new React code

### D3 Code

- Calculate the tree layout, the nodes list and the links list

```javascript
// using d3 to make a tree layout
var treeLayout = d3.layout.tree().size([treeWidth, treeHeight]);
var diagonal = d3.svg.diagonal().projection(function(d) { return [d.x, d.y]; });

// getting tree data
var treeData = ...;

// calculate the nodes list
var nodesList = treeLayout.nodes(treeData).reverse();
var linksList = treeLayout.links(nodesList), function(d) { return d.target.id; };

// append the root svg tag
var rootSvg = d3.select(containerId).append("svg:svg")
    .attr("width", treeWidth)
    .attr("height", treeHeight);
// just a container <g>
var rootGroup = rootSvg.append("svg:g")
    .attr("transform", "translate(" + 0 + "," + 0 + ")");
```

- Render the nodes (not exactly, but similar to this)

```javascript
// 
var nodeGroups = rootGroup.selectAll("g.node")
  .data(nodesList, function(d) { return d.id || (d.id = ++id); });

// enter
var nodeEnter = nodeGroups.enter().append("svg:g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + d.x + "," + d.y + ")"; });
nodeEnter.append("svg:circle")
  .attr("r", 10)
  .style("fill", function(d) { return d._children ? "lightsteelblue" : "#fff"; });
nodeEnter.append("svg:text")
    .text(function(d) { return d.info["fullName"]; })
    .attr("y", -19)
    .attr("dy", ".35em")
    .attr("text-anchor", "middle")
    .style("fill-opacity", 1);
nodeEnter.append("svg:image")
    .attr("xlink:href", function(d){
      return d.info.picture;
    })
    .attr("x", -20)
    .attr("y", -68)
    .attr("height", "40px")
    .attr("width", "40px");

```

- Render the links (not exactly, but similar to this)

```javascript
//
var linksGroup = rootGroup.selectAll("path.link")
      .data(linksList, function(d) { return d.target.id; });
linksGroup.enter().insert("svg:path", "g")
    .attr("class", "link")
    .attr("d", diagonal);
```

### React Code

```
class TreeView extends Component {

  render() {
    /* get the pedigree tree data */
    const { treeData, containerWidth, containerHeight } = this.props;

    /* still use d3.js to calculate the tree layout and position of nodes, links */
    const treeLayout = d3.layout.tree().size([containerWidth, containerHeight]);
    const nodesList = treeLayout.nodes(root).reverse();
    const linksList = treeLayout.links(nodesList);
    const diagonal = d3.svg.diagonal().projection((d) => [d.x, d.y]);

    /* render the nodes */
    const nodes = nodesList.map(node => {
      return (
        <g key={node.id} className="node"
           transform={`translate(${node.x}, ${node.y})`}>
          <circle r="10" style={{'fill': node._children ? 'lightsteelblue' : '#fff'}} />
          <text y="-19" dy=".35em" textAnchor="middle"
                style={{'fillOpacity': 1}}>{node.fullName}</text>
          <image href={node.picture} x="-20" y="-68"
                 width="40px" height="40px"></image>
        </g>
      );
    });

    /* render the links */
    const links = linksList.map(link => {
      return (
        <path key={`${link.source.id}-${link.target.id}`} className="link"
              d={diagonal(link)} />
      );
    });

    return (
      <div className="tree-container">
        <svg height="1000" width={containerWidth}>
          <g>
            {links}
            {nodes}
          </g>
        </svg>
      </div>
    );
  }

}
```

Hmm, much better and easier to understand, especially the svg tag like `<g>`, `<circle>`,
`<path>`,... We can also benefit from React two-way binding and DOM manipulation.

The final result is like this

![tree diagram](/files/2016-07-12-moving-away-from-d3js-im-using-reactjs-for-dom-manipulation-now/tree.png)
