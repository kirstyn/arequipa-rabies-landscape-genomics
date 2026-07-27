<?xml version="1.0" ?>
<sld:StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:gml="http://www.opengis.net/gml" xmlns:ogc="http://www.opengis.net/ogc" xmlns:sld="http://www.opengis.net/sld">
    <sld:UserLayer>
        <sld:LayerFeatureConstraints>
            <sld:FeatureTypeConstraint/>
        </sld:LayerFeatureConstraints>
        <sld:UserStyle>
            <sld:Name>acc_50k</sld:Name>
            <sld:Title/>
            <sld:FeatureTypeStyle>
                <sld:Name/>
                <sld:Rule>
                    <sld:RasterSymbolizer>
                        <sld:Geometry>
                            <ogc:PropertyName>grid</ogc:PropertyName>
                        </sld:Geometry>
                        <sld:Opacity>1</sld:Opacity>
                        <sld:ColorMap>
                            <sld:ColorMapEntry color="#ffffd4" label="30" opacity="1.0" quantity="30"/>
                            <sld:ColorMapEntry color="#fff5c0" label="60" opacity="1.0" quantity="60"/>
                            <sld:ColorMapEntry color="#ffeaac" label="90" opacity="1.0" quantity="90"/>
                            <sld:ColorMapEntry color="#ffdf98" label="120" opacity="1.0" quantity="120"/>
                            <sld:ColorMapEntry color="#fed080" label="240" opacity="1.0" quantity="240"/>
                            <sld:ColorMapEntry color="#febe63" label="360" opacity="1.0" quantity="360"/>
                            <sld:ColorMapEntry color="#feab46" label="480" opacity="1.0" quantity="480"/>
                            <sld:ColorMapEntry color="#fe9929" label="720" opacity="1.0" quantity="720"/>
                            <sld:ColorMapEntry color="#f48821" label="1080" opacity="1.0" quantity="1080"/>
                            <sld:ColorMapEntry color="#e97819" label="1440" opacity="1.0" quantity="1440"/>
                            <sld:ColorMapEntry color="#df6711" label="2880" opacity="1.0" quantity="2880"/>
                            <sld:ColorMapEntry color="#d0590c" label="4320" opacity="1.0" quantity="4320"/>
                            <sld:ColorMapEntry color="#be4c09" label="7200" opacity="1.0" quantity="7200"/>
                            <sld:ColorMapEntry color="#993404" label="12699" opacity="1.0" quantity="12699"/>
                            <sld:ColorMapEntry color="#ab4006" label="108536" opacity="1.0" quantity="108536"/>
                        </sld:ColorMap>
                    </sld:RasterSymbolizer>
                </sld:Rule>
            </sld:FeatureTypeStyle>
        </sld:UserStyle>
    </sld:UserLayer>
</sld:StyledLayerDescriptor>
