/*
 *  projects -- Project operations
 *  Copyright (C) 2016 Stephane Carrez
 *  Written by Stephane Carrez (Stephane.Carrez@gmail.com)
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
var Jason = {};
(function($, undefined) {

    Jason.Create_Task = function(id, url) {
        var container = $('#' + id);
        $(container).panel_box("open", url);
        return false;
    };
/*
    ASF.Actions[''] = function(id, action) {
    };
*/
    /**
     * A collapsible panel using <w:panel>.
     */
    $.widget("ui.panel_box", {
        options: {
            delay: "fast"
        },
        _create: function() {
            var self = this;

            this.element.find(".ui-panel").on('click', "a", function(event) {
                var t = event.target;

                if ($(t).hasClass("ui-icon-closethick") || $(t).hasClass("asf-cancel")) {
                    self.element.children('ul').show(self.options.delay);
                    self.element.children('div').hide(self.options.delay);
                } else if ($(t).hasClass("ui-icon-minusthick")) {
                    self.element.children('ul').show(self.options.delay);
                    self.element.children('div').hide(self.options.delay);
                }
                return false;
            });
        },
        open: function(url) {
            var self = this;
            var target = this.element.find('.asf-container');
            ASF.Update(null, url, target, function(target, jqXHDR) {
                var title = $(target).children('h3');
                if (title.length > 0) {
                    self.element.find('.ui-panel-header .asf-panel-title').html(title.html());
                    title.remove();
                }
                self.element.children('ul').hide(self.options.delay);
                self.element.children('div').show(self.options.delay);
            });
            return false;
        }
    });

})( jQuery );
