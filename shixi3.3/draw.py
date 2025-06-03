
import numpy as np
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
import cartopy.feature as cfeature
from matplotlib.colors import LinearSegmentedColormap
import os
from matplotlib import font_manager


# 设置中文字体
try:
    # 方法1：使用SimHei字体（如果系统中有）
    plt.rcParams['font.sans-serif'] = ['SimHei']  # 用来正常显示中文
    plt.rcParams['axes.unicode_minus'] = False    # 正常显示负号
    print("已设置SimHei作为默认中文字体")
except:
    # 方法2：尝试使用其他可能的中文字体
    chinese_fonts = ['Microsoft YaHei', 'SimSun', 'KaiTi', 'FangSong', 
                    'STSong', 'STZhongsong', 'STFangsong']
    font_found = False
    for font in chinese_fonts:
        if any(f.name == font for f in font_manager.fontManager.ttflist):
            plt.rcParams['font.sans-serif'] = [font]
            plt.rcParams['axes.unicode_minus'] = False
            print(f"已设置{font}作为默认中文字体")
            font_found = True
            break
    
    if not font_found:
        print("警告：未找到中文字体，图表标题可能显示不正确")


# 定义数据参数（来自CTL文件的信息）
nx = 20  # X方向格点数
ny = 16  # Y方向格点数
x_start = 85.0  # 经度起始值
y_start = 32.5  # 纬度起始值
x_step = 3.5    # 经度步长
y_step = 2.5    # 纬度步长

# 生成经纬度网格
lons = np.array([x_start + i * x_step for i in range(nx)])
lats = np.array([y_start + j * y_step for j in range(ny)])
lon_grid, lat_grid = np.meshgrid(lons, lats)

#下面的函数设置需要重点考虑“Fortran使用form='unformatted'写入二进制文件时会在每个记录前后添加记录标记(record markers)，这些标记通常是4字节整数，保存记录长度信息。
#1. ”先读取4字节记录标记（不使用它，但必须读取以跳过）
#2.读取初始场数据并正确重塑为(ny,nx)格式
#3.跳过8字节（尾部记录标记4字节+第二条记录的头部标记4字节）
#4.读取预报场数据并重塑
#5.将两个场组合成所需的数组格式
#这种方法能够正确处理Fortran的二进制记录格式，避免数据错位问题。这也与CTL文件中的XDEF和YDEF定义保持一致，确保地图绘制正确。

def read_binary_data(filepath):
    try:
        with open(filepath, 'rb') as f:
            # 读取记录标记以确定数据大小
            header = np.fromfile(f, dtype=np.int32, count=1)[0]
            # 读取初始场数据
            initial_data = np.fromfile(f, dtype=np.float32, count=nx*ny)
            initial_data = initial_data.reshape(ny, nx)
            
            # 跳过尾部记录标记和第二条记录的头部记录标记
            f.seek(8, 1)
            # 读取预报场数据
            forecast_data = np.fromfile(f, dtype=np.float32, count=nx*ny)
            forecast_data = forecast_data.reshape(ny, nx)
            # 构建最终数据数组
            data = np.zeros((2, ny, nx), dtype=np.float32)
            data[0] = initial_data
            data[1] = forecast_data
            
            return data
            
    except Exception as e:
        print(f"读取错误: {e}")
        return None
def plot_height_field(data, time_idx, title, output_file):

    # 创建图形和坐标系
    fig = plt.figure(figsize=(10, 8))
    ax = plt.axes(projection=ccrs.PlateCarree())
    
    # 添加地图要素
    ax.add_feature(cfeature.COASTLINE)
    ax.add_feature(cfeature.BORDERS, linestyle=':')
    
    # 设置地图范围
    ax.set_extent([lons.min()-1, lons.max()+1, lats.min()-1, lats.max()+1], crs=ccrs.PlateCarree())
    
    # 绘制等值线
    contour = ax.contour(lon_grid, lat_grid, data[time_idx], 
                         levels=np.arange(5000, 6000, 40), 
                         colors='k', 
                         transform=ccrs.PlateCarree())
    
    # 添加等值线标签
    plt.clabel(contour, inline=True, fontsize=8)
    
    # 填充等值线
    contourf = ax.contourf(lon_grid, lat_grid, data[time_idx],
                           levels=np.arange(5000, 6000, 40),
                           cmap='viridis',
                           transform=ccrs.PlateCarree(),
                           alpha=0.6)
    
    # 添加色标
    plt.colorbar(contourf, ax=ax, orientation='horizontal', 
                 label='高度场 (m)', pad=0.05, shrink=0.8)
    
    # 添加网格线
    gl = ax.gridlines(draw_labels=True, linewidth=0.5, 
                     color='gray', alpha=0.5, linestyle='--')
    gl.top_labels = False
    gl.right_labels = False
    
    # 添加标题
    plt.title(title)
    
    # 保存图像
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    plt.close()
    print(f"图像已保存: {output_file}")

def main():
    # 设置工作目录，可以根据需要修改
    data_dir = os.path.expanduser("C:/Users/UNGIFTED/Desktop/shuzhi/shuzhitianqishixi/shixi3.2")
    data_file = os.path.join(data_dir, "h.grd")
    
    # 读取数据
    data = read_binary_data(data_file)
    if data is None:
        return
    
    # 绘制初始场
    plot_height_field(data, 0, "500hPa高度场 - 初始场 (1973-04-29 00Z)", 
                     os.path.join(data_dir, "initialh.png"))
    
    # 绘制预报场
    plot_height_field(data, 1, "500hPa高度场 - 24小时预报 (1973-04-30 00Z)", 
                     os.path.join(data_dir, "predicth.png"))
    
    print("处理完成!")

if __name__ == "__main__":
    main()