// I store all of the in memory data here. Controllers pull from and modify
// this data.
angular.module('App')
  .factory('LocalData', ['$location', '$http', 
    function($location, $http) {

    var contents = {};

    // URL parameters
    var _params = $location.search();

    // Data that R is expecting
    contents.request = {
      language: "en",
      plotWidth: 1000,
      plotType: "Futures",
      yearRange: "full",
      yRange: true,
      date: (new Date()).toISOString().split("T")[0]
    };

    // Contains data for select menus and radio buttons
    contents.forms = {
      yearRange: [{
        text: "All years",
        value: "full"
      }, {
        text: "Last 5 years",
        value: "last5"
      }, {
        text: "Last 10 years",
        value: "last10"
      }, {
        text: "Last 15 years",
        value: "last15"
      }, {
        text: "Last 20 years",
        value: "last20"
      }, {
        text: "Last 25 years",
        value: "last25"
      }]
    };

    return contents;

  }]);
