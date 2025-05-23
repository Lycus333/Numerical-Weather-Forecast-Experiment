
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

def read_binary_data(filepath):
    try:
        # 打印文件大小以便调试
        file_size = os.path.getsize(filepath)
        print(f"文件大小: {file_size} 字节")
        print(f"预期数据大小: {2 * ny * nx * 4} 字节 (2个时间点, {ny}×{nx}网格, 4字节/float32)")
        
        # 读取整个文件
        with open(filepath, 'rb') as f:
            # 方案1: 假设前4个字节是头信息
            data_with_header = np.fromfile(f, dtype=np.float32)
            if len(data_with_header) == 2 * ny * nx + 1:  # 多1个float32值(4字节)
                print("检测到头信息或额外数据，跳过第一个值")
                data = data_with_header[1:]  # 跳过第一个值
            else:
                # 方案2: 尝试直接读取精确字节数
                f.seek(0)  # 回到文件开头
                data = np.fromfile(f, dtype=np.float32, count=2*ny*nx)
        
        print(f"读取数据形状: {data.shape}")
        
        # 重塑为二维数组，2表示有两个时间点
        data = data.reshape(2, ny, nx)
        data = data[:, ::-1, :]  # 沿纬度方向翻转数据
        
        return data
    except Exception as e:
        print(f"读取数据错误: {e}")
        print("尝试进行替代解析...")
        
        try:
            # 尝试另一种读取方式：精确读取所需字节数
            with open(filepath, 'rb') as f:
                # 跳过前4个字节
                f.seek(4)
                data = np.fromfile(f, dtype=np.float32, count=2*ny*nx)
                data = data.reshape(2, ny, nx)
                print("使用替代方法成功读取数据")
                return data
        except Exception as e2:
            print(f"替代解析也失败: {e2}")
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
    data_dir = os.path.expanduser("C:/Users/UNGIFTED/Desktop/shuzhi/shuzhitianqishixi/shixi3")
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