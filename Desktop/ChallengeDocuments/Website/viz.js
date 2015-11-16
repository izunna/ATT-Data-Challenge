var markers = [];
var markerCluster = null;
var a = 'a.png';
var b = 'b.png';
var c = 'c.png';
var d = 'd.png';

var map,pointArray,heatmap;

function initialize() {
  var customStyle = 'custom_style';

  var featureOpts = style();

  var mapOptions = {
    zoom: 5,
    center: new google.maps.LatLng(38.5, -98.0),
    mapTypeControlOptions: {mapTypeIds: [google.maps.MapTypeId.ROADMAP, customStyle]},
    mapTypeId : customStyle
  };

  map = new google.maps.Map(document.getElementById('map-canvas'),
  mapOptions);

  var styledMapOptions = {
        name: 'Custom Style'
    };

  var customMapType = new google.maps.StyledMapType(featureOpts, styledMapOptions);
    map.mapTypes.set(customStyle, customMapType);

  var legend = document.getElementById('legend');
  var legend_styles = [
    {
      name: "Predicted_GradRate < 90. Actual_GradRate < 90",
      icon: a
    },
    {
      name: "Predicted_GradRate < 90. Actual_GradRate >= 90",
      icon: b
    },
    {
      name: "Predicted_GradRate >= 90. Actual_GradRate < 90",
      icon: c
    },
    {
      name: "Predicted_GradRate >= 90. Actual_GradRate >= 90",
      icon: d
    }
  ]

  for (var i = 0; i < legend_styles.length; i++) {
    var name = legend_styles[i].name;
    var icon = legend_styles[i].icon;
    var div = document.createElement('div');
    div.innerHTML = '<img src=' + icon + '>' + name;
    legend.appendChild(div);
  }

  map.controls[google.maps.ControlPosition.RIGHT_BOTTOM].push(
    legend);
  initializeSchools();
}


function style(){
    return [
    {
        "featureType": "landscape",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "lightness": 65
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "poi",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "lightness": 51
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "road.highway",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "road.arterial",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "lightness": 30
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "road.local",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "lightness": 40
            },
            {
                "visibility": "on"
            }
        ]
    },
    {
        "featureType": "transit",
        "stylers": [
            {
                "saturation": -100
            },
            {
                "visibility": "simplified"
            }
        ]
    },
    {
        "featureType": "administrative.province",
        "stylers": [
            {
                "visibility": "off"
            }
        ]
    },
    {
        "featureType": "water",
        "elementType": "labels",
        "stylers": [
            {
                "visibility": "on"
            },
            {
                "lightness": -25
            },
            {
                "saturation": -100
            }
        ]
    },
    {
        "featureType": "water",
        "elementType": "geometry",
        "stylers": [
            {
                "hue": "#ffff00"
            },
            {
                "lightness": -25
            },
            {
                "saturation": -97
            }
        ]
    }
];
}

function initializeSchools() {
    plotAll(schools);
}

function plotAll(nData){
    markers = [];

    for (var key in nData) {
      if (nData.hasOwnProperty(key)) {
        coords_key = key;
        if (key.length < 7) {
          coords_key = "0" + key;
        }
        if (coords.hasOwnProperty(coords_key)) {
          var latlng = new google.maps.LatLng(coords[coords_key]["lat"], coords[coords_key]["lng"]);

          if(nData[key]["prediction"] === 0 && nData[key]["gradRate"] === "0"){
              icon = a; // light green
          } else if (nData[key]["prediction"] === 0 && nData[key]["gradRate"] === "1"){
              icon = b; // orange
          } else if (nData[key]["prediction"] === 1 && nData[key]["gradRate"] === "0") {
              icon = c; // yellow
          }  else {// prediction == 1 && gradRate == 1
              icon = d; // green
          }

          var marker = new google.maps.Marker({
              position: latlng,
              map : map,
               title : coords[coords_key]["name"],
              icon : icon
          });
          markers.push(marker);
        }
      }
    }

    markerCluster = new MarkerClusterer(map, markers);
}

google.maps.event.addDomListener(window, 'load', initialize);
