#Custom Script for GEE Code Editor Night Lights

var viirs_monthly = ee.ImageCollection("NDAA/VIIRS/DNB/MONTHLY_V1/VCMFG")
var annual_composite = viirs_monthly
        .filterDate(" ", "")
        .select("avg_rad")
        .median()
;

var nlVis = {
  min: 0.0,
  max: 1. 
  bands: ['avg_rad'],
};

Map.centerObject(pt_shp);
Map.addLayer(annual_composite,nlVis. "VIIRS annual composite");

//aggregate mean of nightlight intensities for centroid regions of size 256px
var mappedFeatures = pt_shp.map(function(feature){
  var geometry = ee.Geometry.Point([ee.Number(pt_shp.get('lon')), ee.Number(pt_shp.get('lat'))]).buffer(1920).bounds()
  return feature.set(annual_composite.reduceRegion({
    reducer: 'mean',
    geometry: feature.geometry(),
    scale:100,
  }));
});

//Export the Feature ImageCollection
Export.table.toDrive({
  collection: mappedFeatures, 
  description: '',
  fileFormat: 'CSV'
})
