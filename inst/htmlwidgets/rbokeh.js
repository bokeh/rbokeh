HTMLWidgets.widget({

  name: 'rbokeh',

  type: 'output',

  initialize: function(el, width, height) {
    return {
      modelid: "",
      elementid: ""
    }
  },

  renderValue: function(el, x, instance) {

    //clear el for Shiny/dynamic contexts
    el.innerHTML = "";

    if(x.isJSON == true) {
      x.all_models = JSON.parse(x.all_models);
    }

    Bokeh.logger.info("Realizing plot:")
    Bokeh.logger.info(" - modeltype: " + x.modeltype);
    Bokeh.logger.info(" - modelid: " + x.modelid);
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
    // var width = 500;
    // var height = 500;
    // var instance = {
    //   modelid: "80e6788671f9703a699d1f9b98a458d2",
    //   elementid: "1a247d4cb757568cfdf195a517e1f956"
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
  }

});

