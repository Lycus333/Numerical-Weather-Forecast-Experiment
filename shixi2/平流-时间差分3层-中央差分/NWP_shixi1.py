
#%%
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
matplotlib.rcParams['axes.unicode_minus'] = False
font = {'family': 'SimHei'}
matplotlib.rc('font', **font)

# 读取 result.txt 第一行
with open("result.txt", "r") as f:
    first_line = f.readline().strip()  # 去除首尾空格和换行符
# 加载数据（跳过第一行）
data = np.loadtxt("result.txt", skiprows=1)
x = data[0:800, 0]
u0 = data[0:800, 1]
u = data[0:800, 2]
u_analytic = data[0:800, 6]
# ======================================================================
# 可视化（修改标题部分）
# ======================================================================
plt.figure(figsize=(10, 6))
plt.plot(x, u0, 'k--', label='Initial')
plt.plot(x, u, 'r-', lw=2, label='Analytic')
plt.plot(x, u_analytic, 'b--', label='Numerical')
plt.xlabel('x')
plt.ylabel('u')
plt.title(f'时间中央差 空间中央差 | 参数: {first_line}')  # 添加第一行内容
plt.legend()
plt.grid(alpha=0.3)
plt.show()



# # %%
# import matplotlib.pyplot as plt
# print(plt.rcParams['font.sans-serif'])  # 应输出 ['SimHei', ...]

# # %%
# import matplotlib.font_manager

# # 或执行绘图操作触发自动重建
# import matplotlib.pyplot as plt
# plt.plot([1,2,3])
# plt.show()

# %%
