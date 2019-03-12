
# coding: utf-8

# In[1]:


import folium
from folium import plugins
from folium.plugins import HeatMap
import pandas as pd
import pickle

# In[8]: Subsetting for Vegas

vegas = pd.read_csv('vegas.csv')
vegas = vegas[vegas['Las Vegas']==1]
vegas = vegas[vegas['cat_Restaurants']==1]
pickle.dump(vegas, open( "Vegas_subset.p", "wb" ) )

vegas_high5 = vegas.loc[vegas['stars'] == 5.0, ['business_id','latitude','longitude']]
vegas_high45 = vegas.loc[vegas['stars'] == 4.5, ['business_id','latitude','longitude']]
vegas_high4 = vegas.loc[vegas['stars'] == 4.0, ['business_id','latitude','longitude']]
vegas_high = pd.merge(vegas_high5, vegas_high45, how='outer')
vegas_high = pd.merge(vegas_high, vegas_high4, how='outer')
pickle.dump(vegas_high, open( "Vegas_high.p", "wb" ) )

vegas_low1 = vegas.loc[vegas['stars'] == 1.0, ['business_id','latitude','longitude']]
vegas_low15 = vegas.loc[vegas['stars'] == 1.5, ['business_id','latitude','longitude']]
vegas_low2 = vegas.loc[vegas['stars'] == 2.0, ['business_id','latitude','longitude']]
vegas_low = pd.merge(vegas_low1, vegas_low15, how='outer')
vegas_low = pd.merge(vegas_low, vegas_low2, how='outer')
pickle.dump(vegas_low, open( "Vegas_low.p", "wb" ) )

#%% 
pickle.dump(vegas, open( "Vegas_subset.p", "wb" ) )
pickle.dump(vegas_high, open( "Vegas_high.p", "wb" ) )
pickle.dump(vegas_low, open( "Vegas_low.p", "wb" ) )
# In[7]:

hmap = folium.Map(
    location=[36.1699, -115.1398],
    zoom_start=10,
)

#%% Vegas low and high star heat maps
hm_wide = HeatMap(
    list(
        zip(vegas_low['latitude'].values,
            vegas_low['longitude'].values)),
    min_opacity=0.2,
    max_val=1,
    radius=10,
    blur=18,
    max_zoom=1,
)
hm_wide.add_to(hmap)
#hmap.save('maps/heatmap_vagas.html')
hmap.save('maps/heatmap_Vegas_Stars_low.html')


hm_wide = HeatMap(
    list(
        zip(vegas_high['latitude'].values,
            vegas_high['longitude'].values)),
    min_opacity=0.2,
    max_val=1,
    radius=10,
    blur=18,
    max_zoom=1,
)
hm_wide.add_to(hmap)
#hmap.save('maps/heatmap_vagas.html')
hmap.save('maps/heatmap_Vegas_Stars_high.html')

# In[8]: Subsetting for Phoenix

phoenix = pd.read_csv('Phoenix.csv')
phoenix = phoenix[phoenix['Phoenix']==1]
phoenix = phoenix[phoenix['cat_Restaurants']==1]
pickle.dump(phoenix, open( "Phoenix_subset.p", "wb" ) )

phoenix_high5 = phoenix.loc[phoenix['stars'] == 5.0, ['business_id','latitude','longitude']]
phoenix_high45 = phoenix.loc[phoenix['stars'] == 4.5, ['business_id','latitude','longitude']]
phoenix_high4 = phoenix.loc[phoenix['stars'] == 4.0, ['business_id','latitude','longitude']]
phoenix_high = pd.merge(phoenix_high5, phoenix_high45, how='outer')
phoenix_high = pd.merge(phoenix_high, phoenix_high4, how='outer')
pickle.dump(phoenix_high, open( "Phoenix_high.p", "wb" ) )

phoenix_low1 = phoenix.loc[phoenix['stars'] == 1.0, ['business_id','latitude','longitude']]
phoenix_low15 = phoenix.loc[phoenix['stars'] == 1.5, ['business_id','latitude','longitude']]
phoenix_low2 = phoenix.loc[phoenix['stars'] == 2.0, ['business_id','latitude','longitude']]
phoenix_low = pd.merge(phoenix_low1, phoenix_low15, how='outer')
phoenix_low = pd.merge(phoenix_low, phoenix_low2, how='outer')
pickle.dump(phoenix_low, open( "Phoenix_low.p", "wb" ) )
#%% 
pickle.dump(phoenix, open( "Phoenix_subset.p", "wb" ) )
pickle.dump(phoenix_high, open( "Phoenix_high.p", "wb" ) )
pickle.dump(phoenix_low, open( "Phoenix_low.p", "wb" ) )
# In[7]:

hmap = folium.Map(
    location=[33.4484, -112.0740],
    zoom_start=10,
)


#%% Vegas low and high star heat maps
hm_wide = HeatMap(
    list(
        zip(phoenix_low['latitude'].values,
            phoenix_low['longitude'].values)),
    min_opacity=0.2,
    max_val=1,
    radius=10,
    blur=18,
    max_zoom=1,
)
hm_wide.add_to(hmap)
#hmap.save('maps/heatmap_vagas.html')
hmap.save('maps/heatmap_Phoenix_Stars_low.html')


hm_wide = HeatMap(
    list(
        zip(phoenix_high['latitude'].values,
            phoenix_high['longitude'].values)),
    min_opacity=0.2,
    max_val=1,
    radius=10,
    blur=18,
    max_zoom=1,
)
hm_wide.add_to(hmap)
#hmap.save('maps/heatmap_vagas.html')
hmap.save('maps/heatmap_Phoenix_Stars_high.html')

#%% 

markermap = folium.Map(location=[36.1699, -115.1398], zoom_start=14)
plugins.MarkerCluster(
    list(
        zip(vegas['latitude'].values,
            vegas['longitude'].values)),
    popups = [folium.Popup(str(x), parse_html=True) for x in vegas['name'].values]
).add_to(markermap)
markermap.save('maps/marker2.html')

