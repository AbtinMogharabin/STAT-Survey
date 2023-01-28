import matplotlib.pyplot as plt
import seaborn as sns

'''bar_data={'variance 1':0.215238,
          'variance 2':0.2045346,
          'variance 3':0.226538,
          'variance 4':0.1984465,
          'variance 5':0.2023358,
          'variance 6':0.2282899}
'''
bar_data={'variance 1':0.2115125,
          'variance 2':0.2208134,
          'variance 3':0.2081364,
          'variance 4':0.2207102,
          'variance 5':0.1896092,
          'variance 6':0.1952177}
sns.set_theme()

x1 = list(bar_data.keys())
y1 = list(bar_data.values())
sns.set(font_scale=5)
sns.set(rc={'figure.figsize':(11.7,8.27)})

sns.barplot( x=x1, y=y1)
plt.show()
