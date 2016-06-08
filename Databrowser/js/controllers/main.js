// Controls controller
// Exposes the model to the template
// Contains plot options, map, and overlay options
angular.module('App')
  .controller('Main', ['$scope', '$filter', 'LocalData', 'GetJSON',
    function($scope, $filter, LocalData, GetJSON, Utils) {



    //////////////////////////
    //----------------------//
    // Assign data to scope //
    //----------------------//
    //////////////////////////

    $scope.request = LocalData.request;

    $scope.plotType = LocalData.forms.plotType;

    $scope.yearRange = LocalData.forms.yearRange;

    $scope.date = new Date();

    $scope.$watch("date", function(date) {
        $scope.request.date = date.toISOString().split("T")[0];
    })


    ///////////////////////////
    //-----------------------//
    // Plot options controls //
    //-----------------------//
    ///////////////////////////

    // Make a request to the server
    $scope.updatePlot = function() {
      GetJSON.get(LocalData.request)
        .then(function(result) {
          LocalData.result = result;
        });
    };

    // This ensures that too many requests are not made
    var events = [];

    // Initial plot on load
    // watch 'request' and if it changes execute the function
    $scope.$watch('[request]', function(){

        // Give this event an ID
        var id = events.length;
        events.push(id);

        window.setTimeout(function(){
          var last = events.slice(-1)[0]
          // If this events ID is the most recent ID, update the plot
          if(id === last) {
            $scope.updatePlot();
          }
        }, 200);
      
    }, true)




    ///////////////////
    //---------------//
    // Plot controls //
    //---------------//
    ///////////////////

    // Current status, i.e. "loading" or error message
    $scope.status = GetJSON.status;

    // popup starts as invisible
    $scope.popup = {
      visible: false
    }

    // Watched for changes in plotURL, which translates to this function firing
    // whenever a successful request is made
    $scope.$watch(function() {
      return LocalData.result;
    }, function() {
      if(LocalData.result) {
        $scope.url = LocalData.result.data.rel_base + ".png"
        $scope.popup.url = $scope.url
      }
    });

  }]);