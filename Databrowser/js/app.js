// Main application.

// Define our application, 'App', and list module dependencies
var app = angular.module('App',
                         ['ngRoute', 'ngSanitize', 'ui.select', 'ngMaps', 'ui.slider', 'ngAnimate', 'ui.bootstrap'])

// Configure URL processing including routing and parameter handling
.config(['$routeProvider', '$locationProvider', function($routeProvider, $locationProvider) {

    // Set up routing
    $routeProvider.
        when('/', {
            templateUrl: 'templates/main.html'
        }).
        when('/about', {
            templateUrl: 'templates/about.html'
        }).
        otherwise({
            redirectTo: '/'
        });


    // Allow parameter=value pairs to be read from the URL
    // $locationProvider.html5Mode(true);

}]);
