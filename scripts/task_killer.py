import paramiko
import sys

def ssh_execute_command(host, command):
    try:
        client = paramiko.SSHClient()
        client.set_missing_host_key_policy(paramiko.AutoAddPolicy())

        # 直接使用 SSH 配置的主机名连接，无需指定用户名、密钥
        client.connect(hostname=host)

        stdin, stdout, stderr = client.exec_command(command)

        output = stdout.read().decode()
        error = stderr.read().decode()

        client.close()

        if error:
            print(f"[{host}] Error: {error}")
        else:
            print(f"[{host}] Output: {output}")
    except Exception as e:
        print(f"[{host}] Exception: {e}")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python ssh_script.py <hosts>")
        sys.exit(1)

  
