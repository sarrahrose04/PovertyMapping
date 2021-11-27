#Obtain mean luminosity values from night lights

var annual_composite = viirs_annual
        select('b1') //Average DNB radiance values  
;
//Define var nlVis to store map visualisation parameters
var nlVis = {
  min:0.0,
  max: 1, 
  bands: ['b1'],
};

//Use grid centroid shapefile to put map view in centre
Map.centerObject(pt_shp);
//Visualise b1 band of the viirs_annual raster using visualisation parameters
Map.addLayer(annual_composite,nlVis,"NTL annual composite");


//Define luminosity aggregation function, which takes centroid and creates a circle buffer around it with radius half grid size 
var mappedFeatures = pt_shp.map(function(feature) {
  var geometry = ee.Geometry.Point([ee.Number(pt_shp.get('lon')), ee.Number(pt_shp.get('lat'))].buffer(1920).bounds()
  //reduceRegion function returns avg of luminosity values within buffer boundary
  return feature.set(annual_composite.reduceRegion({
    reducer: 'mean'
    geometry: feature.geometry(),
    scale:100,
  }));
});

//Export attribute table of shapefile as CSB file into Google Drive
Export.table.toDrive({
  collection: mappedFeatures, 
  description: "",
  fileFormat: 'CSV'
});
