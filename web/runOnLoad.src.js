/*

runOnLoad.js

Runs tasks when the DOM content has loaded

Created by Stephen Morley - http://code.stephenmorley.org/ - and released under
the terms of the CC0 1.0 Universal legal code:

http://creativecommons.org/publicdomain/zero/1.0/legalcode

*/

var runOnLoad = (function(){

  // set that the tasks have not yet run
  var tasksRun = false;

  // initialise the task list
  var tasks = [];

  // Adds a task. The parameter is:
  //
  // task - the task
  function add(task){

    // check whether tasks have been run
    if (tasksRun){

      // run the task asynchronously
      window.setTimeout(task, 0);

    }else{

      // add the task to the list
      tasks.push(task);

    }

  }

  // Runs the tasks.
  function runTasks(){

    // set that the tasks have run
    tasksRun = true;

    // loop over the tasks, running them
    while (tasks.length) tasks.shift()();

  }

  // check which method of adding event listeners is supported
  if (window.addEventListener){

    // listen for the DOMContentLoaded event
    document.addEventListener('DOMContentLoaded', runTasks, false);

    // listen for the load event
    window.addEventListener('load', runTasks, false);

  }else{

    // listen for the load event
    window.attachEvent('onload', runTasks);

  }

  // return the public API
  return add;

})();
