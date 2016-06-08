// Service for making JSON requests. Also provides access to the request status, i.e. 
// loading or error messages
angular.module('App')
  .factory('GetJSON', ['$http', '$q', 
    function($http, $q) {

    // Databrowser cgi url, this is always the same
    var _url = '/cgi-bin/__DATABROWSER__.cgi?';

    // Serialize object
    var _serialize = function(obj) {
      var str = "";
      for (var key in obj) {
        if (str !== "") {
          str += "&";
        }
        str += key + "=" + obj[key];
      }
      return str;
    };

    // Error handling (this won't usually happen)
    var _error = function(response) {
      return ($q.reject(response.data.message));
    };

    // Success handling. This includes handling errors that are
    // successfuly returned.
    var _success = function(response) {
      status.loading = false;
      if (response.data.status === "ERROR") {
        status.error = response.data.error_text;
        return ($q.reject(response.data.error_text));
      }
      return (response);
    };

    // Current status
    var status = {
      loading: false,
      error: false
    };

    // Make a json request with an object of data
    var get = function(data) {
      status.loading = true;
      status.error = false;
      var request = $http({
        method: 'POST',
        url: _url + _serialize(data)
      });
      return (request.then(_success, _error));
    };

    return {
      status: status,
      get: get
    };

  }]);