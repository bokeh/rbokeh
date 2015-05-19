HTMLWidgets.widget({

  name: 'rbokeh',

  type: 'output',

  initialize: function(el, width, height) {
    return {
      modelid: "",
      elementid: "",
      width: width,
      height: height
    }
  },

  renderValue: function(el, x, instance) {

    //clear el for Shiny/dynamic contexts
    el.innerHTML = "";

    if(x.isJSON == true) {
      x.all_models = JSON.parse(x.all_models);
    }

    // set size from initialize if "figure" (doesn't work for gridplot now)
    if(x.padding.type == "figure") {
      if(instance.width) {
        x.all_models[0].attributes.plot_width = instance.width - x.padding.y_pad;
      }
      if(instance.height) {
        x.all_models[0].attributes.plot_height = instance.height - x.padding.x_pad;
      }
    }

    if (HTMLWidgets.shinyMode)
      this.addActionCallbackShinyInput(el.id, x);

    Bokeh.logger.info("Realizing plot:")
    Bokeh.logger.info(" - modeltype: " + x.modeltype);
    Bokeh.logger.info(" - modelid:   " + x.modelid);
    Bokeh.logger.info(" - elementid: " + x.elementid);

    instance.modelid = x.modelid;
    instance.elementid = x.elementid;

    if(x.debug == true) {
      console.log(x.all_models);
      console.log(JSON.stringify(x.all_models));
    }

    var dv = document.createElement('div');
    dv.id = x.elementid;
    dv.setAttribute("class", "plotdiv");
    el.appendChild(dv);

    Bokeh.load_models(x.all_models);
    var model = Bokeh.Collections(x.modeltype).get(x.modelid);
    var view = new model.default_view({model: model, el: '#' + x.elementid});

    // Bokeh.instance = view;
    Bokeh.index[x.modelid] = view;
  },

  resize: function(el, width, height, instance) {
    // var width = 800;
    // var height = 500;
    // var instance = {
    //   modelid: "4f205d18bb3373d73ba892f2eac70640",
    //   elementid: "73b93d6f1c812a87ac5621bc622762e0"
    // };

    var box = document.getElementById(instance.elementid).getElementsByTagName("table")[0];

    if(Bokeh.index[instance.modelid].model.attributes.children) {
      // it is a gridplot (TODO)
      // need to get dimensions and set dimensions of all plots
      // Bokeh.index[instance.modelid].child_views["_id_"].canvas._set_dims([300,300])
    } else {
      // it's a regular plot
      var bk_canvas = document.getElementById(instance.elementid).getElementsByClassName("bk-canvas-wrapper")[0];
      var h_pad = box.clientHeight - bk_canvas.clientHeight;
      var w_pad = box.clientWidth - bk_canvas.clientWidth;

      // TODO: also shrink font sizes, etc., to a certain degree?
      Bokeh.index[instance.modelid].canvas._set_dims([width - w_pad,height - h_pad]);
    }
  },
  
  addActionCallbackShinyInput : function(id, x) {
    var ind = -1;
    for(var i = 0; i < x.all_models.length; i += 1) {
        if(x.all_models[i].type === "Callback") {
            ind = i;
        }
    }

    if (ind > -1) {
      // check for an existing actionCallback
      var prevActionCallback = x.all_models[ind].attributes.code;

      // install the callback
      if (!prevActionCallback)
        x.all_models[ind].attributes.code = "rslt = getBokehSelected(cb_obj);" +
          "var input_id = '" + id + "_action';" +
          "Bokeh.logger.info('Sending data to shiny: ' + input_id);" +
          "Shiny.onInputChange(input_id, rslt);";
    } else {
      Bokeh.logger.info('No callback model present');
    }
  }

});

getBokehSelected = function(cb_obj) {
  var inds = cb_obj.get('selected')['1d'].indices;
  var data = cb_obj.get('data');

  var rslt = {};
  for (i = 0; i < inds.length; i++) {
    for (var key in data) {
      if (data.hasOwnProperty(key)) {
        rslt[key] = data[key][inds[i]];
      }
    }
  }

  return(rslt);
};