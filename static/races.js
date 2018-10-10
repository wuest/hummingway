function openRace(i)
{
  document.location.href = "/race/" + i;
}

function viewTracker(i)
{
  document.location.href = "/racer/" + i;
}

function startTracker(player, tracker)
{
  var canvas = document.getElementById("trackerCanvas");
  getRidOfThis_getTrackerLayout(canvas, tracker);
  setInterval(getRidOfThis_updateTracker(canvas, player), 1000);
}

function getRidOfThis_getTrackerLayout(canvas, tid)
{
  var req = new XMLHttpRequest();
  var uri = "/api/0.1.0/tracker/" + tid
  req.onreadystatechange = function()
  {
    if(this.readyState == 4 && this.status == 200)
    {
      trackerRender(canvas, JSON.parse(this.responseText));
    }
  }
  req.open("GET", uri, true);
  req.send();
}

function getRidOfThis_updateTracker(canvas, player)
{
  return(function() {
    var req = new XMLHttpRequest();
    var uri = "/api/0.1.0/racer/" + player
    req.onreadystatechange = function()
    {
      if(this.readyState == 4 && this.status == 200)
      {
        trackerUpdate(canvas, JSON.parse(this.responseText));
      }
    }
    req.open("GET", uri, true);
    req.send();
  })
}

function trackerRender(canvas, json)
{
  var t = document.createElement("table");

  for(var i = 0; i < json[2].length; i++)
  {
    var r = document.createElement("tr");
    var rowJ = json[2][i][2];
    for(var j = 0; j < rowJ.length; j++)
    {
      var d = document.createElement("td");
      var di = document.createElement("img");
      di.src = rowJ[j][1]["cellIcon"];
      d.className = "inactive";
      d.id = "cell_" + rowJ[j][0];
      d.appendChild(di);
      r.appendChild(d);
    }
    t.appendChild(r);
  }
  canvas.appendChild(t);
}

function trackerUpdate(canvas, json)
{
  var state = json[2]["playerState"];
  if(state == "")
  {
    state = [];
  }
  else
  {
    state = JSON.parse(state);
  }

  for(var i = 0; i < state.length; i++)
  {
    var cid = state[i]["cell"];
    document.getElementById("cell_" + cid).className = state[i]["status"];
  }
}
