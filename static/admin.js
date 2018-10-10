function openRace(i)
{
  document.location.href = "/admin/race/" + i;
}

function openTracker(i)
{
  document.location.href = "/admin/tracker/" + i;
}

function addRow(i)
{
  document.location.href = "/admin/tracker/" + i + "/addrow";
}

function deleteRow(i)
{
  var ref = document.location.href.split('/');
  var r   = ref[ref.length - 1];
  document.location.href = "/admin/row/" + i + "/del/" + r;
}

function addCell(i)
{
  var ref = document.location.href.split('/');
  var r   = ref[ref.length - 1];
  document.location.href = "/admin/row/" + i + "/addcell/" + r;
}

function delCell(i)
{
  var r = document.getElementById('tracker').value;

  if(window.confirm("Really delete cell?"))
  {
    document.location.href = "/admin/cell/" + i + "/del/" + r;
  }
}

function editCell(i)
{
  var ref = document.location.href.split('/');
  var r   = ref[ref.length - 1];
  document.location.href = "/admin/cell/" + i + "/" + r;
}

function startTracker(race)
{
  //var canvas = document.getElementById("trackerCanvas");
  //getRidOfThis_getTrackerLayout(canvas, tracker);
  //setInterval(getRidOfThis_updateTracker(canvas, player), 1000);
}

function toggle(item, player)
{
  var canvas = document.getElementById("racer_" + player);
  var tds = canvas.getElementsByTagName("table")[0].getElementsByTagName("td"); // Gross, don't ask.
  var blob = [];
  for(var i = 0; i < tds.length; i++)
  {
    var name = "cell_" + tds[i].id;
    var state = tds[i].className;

    if(item == tds[i].id)
    {
      if(state == "inactive")
      {
        tds[i].className = "";
        state = "";
      }
      else
      {
        tds[i].className = "inactive";
        state = "inactive";
      }
    }

    blob.push({"cell":tds[i].id, "status":state});
  }
  getRidOfThis_updateTracker(player, JSON.stringify(blob));
}

function getRidOfThis_updateTracker(player, blob)
{
  var req = new XMLHttpRequest();
  var uri = "/admin/state";

  req.open("POST", uri, true);
  req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
  req.send(encodeURI("player=" + player + "&blob=" + blob));
}
