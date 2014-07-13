/*
 * This javascript file supports the robot_control.html page.
 * It was pulled out of the html page itself when the page
 * was mustache-templated. For some reason, the mustache
 * engine kind of whacks things up. It's better to separte
 * the javascript anyway IMHO.
 */
var wsUri = "wss://daneel:8081/";

function addRobotOption(robot, selected) {
  var options = getCurrentRobotOptions();
  options.push(robot);
  return formatOptions(options, selected);
}

function removeRobotOption(robot, selected) {
  var options = getCurrentRobotOptions();
  var indexOfRobot = options.indexOf(robot);
  options.splice(indexOfRobot, 1);
  return formatOptions(options, selected);
}

function formatOptions(options, selected) {
  var optionHtml = "";
  for (var i=0; i<options.length; i++) {
    optionHtml += "<option value='" + options[i] + "'";
    if (options[i] === selected) {
      optionHtml += " selected";
    }
    optionHtml += ">" + options[i] + "</option>";
  }
  return optionHtml;
}

function getCurrentRobotOptions() {
  var options = $('#robot_id option');

  var values = $.map(options ,function(option) {
      return option.value;
    });
  return values;
}

/**
 * Issue the robot a command
 */
function issueCommand(command) {
  var robotId = $( "#robot_id" ).val()
  var re = /is not registered/;
  if ( robotId ) {
    $.ajax({url: "/robot/commands/"+command+"?robot="+robotId, success:function(result) {
        if (result.match(re)) {
          alert(result);
        } else {
          $("#div_result").html(result);
        }
    }});
  } else {
    alert("Must specify a robot id")
  }
}

var ws = $.websocket(wsUri, {
  open: function(e) {
    $("#output").append("CONNECTED");
  },
  close: function(e) {
    $("#robot_id").val("");
    $("#output").append("DISCONNECTED")
  },
  events: {
    register: function(e) {
      var selected = $("#robot_id").val();
      var robot_name = e.data.name;
      var optionHtml = addRobotOption(robot_name, selected);
      $("#robot_id").empty().append(optionHtml);
      $("#output").append('<br><span style="color: blue;">REGISTER: ' + e.data.name + "</span><br>") 
    },
    unregister: function(e) {
      var selected = $("#robot_id").val();
      var robot_name = e.data.name;
      var optionHtml = removeRobotOption(robot_name, selected)
      $("#robot_id").empty().append(optionHtml);
      $("#output").append('<br><span style="color: blue;">UNREGISTER: ' + e.data.name + "</span><br>") 
    }
  }
});


/**
 * Register the click-events to send the commands.
 */
$( document ).ready(function() {

  // Handle the slow-down button
  $("#slow_down" ).click(function(event) { issueCommand("slow_down") });
  // Handle the forward button
  $("#forward" ).click(function(event) { issueCommand("forward") });
  // Handle the speed up button
  $("#speed_up" ).click(function(event) { issueCommand("speed_up") });
  // Handle the rotate_ccw up button
  $("#rotate_ccw" ).click(function(event) { issueCommand("rotate_ccw") });
  // Handle the stop button
  $("#stop" ).click(function(event) { issueCommand("stop") });
  // Handle the rotate_cw button
  $("#rotate_cw" ).click(function(event) { issueCommand("rotate_cw") });
  // Handle the backward button
  $("#backward" ).click(function(event) { issueCommand("backward") });
});

